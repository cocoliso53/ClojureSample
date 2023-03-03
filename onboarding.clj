(ns website.routes.onboarding
  (:require
   [website.layout :as layout]
   [website.middleware :as middleware]
   [website.emails :as emails]
   [website.db.cloud-core :as dat]
   [com.brunobonacci.mulog :as u]
   [ring.util.response]
   [clj-uuid :as uuid]
   [website.auth :as auth]
   [ring.util.http-response :as response]))

;;; --- helpers --- ;;;

(defn clean-request
  "Function thar pre-selects only relevant request data to be logged"
  [request & ks]
  (reduce dissoc request (concat [:session/key :reitit.core/match :reitit.core/router :body
                                  :muuntaja/response :server-exchange]
                                 ks)))

(defn get-session
  "Get session ID from request's cookie"
  [request]
  (get-in request [:cookies "JSESSIONID" :value]))

(defn valid-user?
  "Is the user fully validated?"
  [email]
  (let [validations (dat/get-user-validated (dat/get-db dat/conn) email)]
    (reduce #(and %1 %2) (first validations))))


(defn token-expired?
  "Checks if the errors from a token uuid is expired to resend another"
  [errors]
  (->> errors
       nfirst
       first
       (= "El link ya expiró")))


;;; ---  Register --- ;;;
(defn register-page
  "GET method. Shown when a new user is creating their account"
  [{:keys [session flash] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request :flash)
                   :flash flash}
    (if-let [user (get-in session [:identity :user/id])]
      (do
        (u/log ::register :user user)
        (ring.util.response/redirect "/confirm"))
      (do
        (u/log ::register :errors (:errors flash))
        (layout/render request "register.html"
                       {:errors (:errors flash)
                        :data (:data flash)})))))

(defn validate-register
  "POST method. Validates params for new user are correct. If they are then
   creates the user on the db and sends an email with link for email
   verification."
  [{:keys [params] :as request}]
  (u/with-context {:params (dissoc params :psw :pwd)
                   :request (clean-request request :params :form-params)
                   :session/key (get-session request)}
    (if-let [errors (auth/validate-create-user params)]
      (do
        (u/log ::validate-register :errors errors)
        (assoc (ring.util.response/redirect "/user/welcome")
               :flash {:errors errors
                       :data params}))
      (let [email (:email params)
            token (uuid/v1)
            new-user (auth/create-user params)
            new-token (auth/create-token email token)]
        (u/log ::validate-register :email email
               :tx-data-token (distinct (map :e (:tx-data new-token)))
               :tempids-token (:tempids new-token)
               :tx-data-user (distinct (map :e (:tx-data new-user)))
               :tempids-user (:tempids new-user)
               :send-email (emails/send-validation-email email token)); future? 
        (assoc (ring.util.response/redirect "/user/id-upload")
               :session {:identity {:preuser (:email params)}
                          :last-activity (System/currentTimeMillis)})))))

(defn id-upload
  "GET method. User who is creating a new account gets redirected here.
   Checks if :session :preuser is not nill, if it is redirects to register,
   else continue as normal"
  [{:keys [session flash] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request :session/key)}
    (if-let [preuser (get-in session [:identity :preuser])]
      (do
        (u/log ::id-upload :preuser preuser)
        (layout/render request "id-upload.html"
                       {:preuser preuser
                        :errors (:errors flash)
                        :data (:data flash)}))
      (do
        (u/log ::id-upload)
        (ring.util.response/redirect "/user/welcome")))))


(defn validate-id
  "POST method. Will create :id on db and preliminary check on ID data.
   Sends an email to us to manually verify ID."
  [{:keys [session params] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request :params :form-params)
                   :params params}
    (if-let [errors (auth/validate-create-id params)]
      (do
        (u/log ::validate-id :errors errors)
        (assoc (ring.util.response/redirect "/user/id-upload")
               :flash {:errors errors
                       :data params}))
      (let [email (get-in session [:identity :preuser])
            id-tx (auth/create-id email params)]
        (u/log ::validate-id :send-email (emails/send-id-alert email params)
               :tx-data (distinct (map :e (:tx-data id-tx)))
               :tempids (:tempids id-tx))
        (assoc (ring.util.response/redirect "/user/wait")
               :flash {:id false})))))

(defn validate-email
  "GET request. Validates if the token in =path-params= belongs to a user with
   no validated email. If condition is true then email is validated."
  [{:keys [path-params flash] :as request}]
  (u/with-context {:path-params path-params
                   :request (clean-request request :path-params)
                   :session/key (get-session request)}
    (if-let [errors (auth/validate-recovery-token path-params)]
      (do
        (u/log ::validate-email :errors errors)
        (layout/render request "general-error.html"
                       {:errors errors
                        :token (if (token-expired? errors)
                                 (:token path-params))}))
      (let [t-m {:token-s (:token path-params)}
            tx-update-token (auth/update-recovery-token t-m)
            tx-update-user (auth/update-user-email t-m)]
        (u/log ::validate-email
               :tx-update-token (distinct (map :e (:tx-data tx-update-token)))
               :tempids-update-token (:tempids tx-update-token)
               :tx-update-user (distinct (map :e (:tx-data tx-update-user)))
               :tempids-update-user (:tempids tx-update-user))
        (layout/render request "email-validated-ok.html")))))

(defn resend-validate-email
  "POST request. Given a token, it looks up the email of it's owner,
   generates a new token, and sends an email with the new token info"
  [{:keys [params] :as request}]
  (u/with-context {:params params
                   :request (clean-request request :params :form-params)
                   :session/key (get-session request)}
    (if-let [email (ffirst (dat/get-token-email (dat/get-db dat/conn) (:token params)))]
      (let [token (uuid/v1)
            new-token (auth/create-token email token)]
        (u/log ::resend-validate-email :email email
               :tx-data-token (distinct (map :e (:tx-data new-token)))
               :tempids-token (:tempids new-token)
               :send-email (emails/send-validation-email email token))
        (ring.util.response/redirect "/user/instructions"))
      (do
        (u/log ::resend-validate-email :errors "No email associated with this token"
               :token (:token params))
        (ring.util.response/redirect "/user/error")))))


(defn retry-id
  "GET method. To be used when a first verification of ID fails and
   user needs to reupload their information."
  [{:keys [session flash] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request :flash)
                   :flash flash}
    (if-let [user (get-in session [:identity :user/id])]
      (do
        (u/log ::retry-id :user user)
        (ring.util.response/redirect "/confirm"))
      (do
        (u/log ::retry-id :errors (:errors flash))
        (layout/render request "retry-id.html"
                       {:errors (:errors flash)})))))

(defn validate-retry-id
  "POST method. Checks if the email introduced is a valid one
   and if the user doesn't have their ID validated."
  [{:keys [params session] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request :params)
                   :params params}
    (if-let [errors (auth/validate-reset-id params)]
      (do
        (u/log ::validate-retry-id :errors errors)
        (assoc (ring.util.response/redirect "/user/retry-id") ; check
               :flash {:errors errors}))
      (do
        (u/log ::validate-retry-id)
        (assoc (ring.util.response/redirect "/user/id-upload")
               :session {:identity {:preuser (:email params)}})))))

(defn validate-wait
  "GET request. It is shown to the user when they try to login and
   they haven't verified their email and/or id"
  [{:keys [flash] :as request}]
  (u/with-context {:session/key (get-session request)
                   :request (clean-request request)
                   :flash flash}
    (if (empty? flash)
      (do
        (u/log ::validate-wait)
        (ring.util.response/redirect "/"))
      (do
        (u/log ::validate-wait)
        (layout/render request "wait.html"
                       {:id (:id flash)
                        :email (:email flash)})))))

;;; --- Password recovery --- ;;;

(defn recovery-email-address
  "GET method. Renders page to enter email address where a passwrod recovery link will be sent.
   If a user logged in is trying to access this page then gets redirected."
  [{:keys [session flash] :as request}]
  (u/with-context {:flash flash
                   :session/key (get-session request)
                   :request (clean-request request :flash)}
    (if-let [user (get-in session [:identity :user/id])]
      (do
        (u/log ::recovery-email-address :user user)
        (ring.util.response/redirect "/confirm"))
      (do
        (u/log ::recovery-email-address :errors (:errors flash))
        (layout/render request "recover-password.html"
                       {:errors (:errors flash)})))))

(defn create-recovery-token
  "POST method. Receives a request from =recovery-email-address= containing an email address
   as in =params=. If the =email= exists then creates a =token= and sends an email with a
   link to recover the password. Otherwise redirects to =/user/forgot= displaying errors."
  [{:keys [params] :as request}]
  (u/with-context {:params (dissoc params :__anti-forgery-token)
                   :session/key (get-session request)
                   :request (clean-request request :params :form-params)}
    (if-let [errors (auth/validate-existing-email params)]
      (do
        (u/log ::create-recovery :errors errors)
        (assoc (ring.util.response/redirect "/user/forgot")
               :flash {:errors errors}))
      (let [email (:email params)
            token (uuid/v1)
            new-token (auth/create-token email token)]
        (u/log ::create-recovery :email email
               :tx-data (distinct (map :e (:tx-data new-token)))
               :tempids (:tempids new-token)
               :send-email (emails/send-recovery-email email token))
        (layout/render request "recover-email.html")))))

(defn choose-password
  "Get method. Renders the page to reset and choose a new password if the `token` in
    `paths-parmas` is valid. Otherwise renders an error page."
  [{:keys [path-params flash] :as request}]
  (u/with-context {:path-params path-params
                   :request (clean-request request :path-params)
                   :session/key (get-session request)}
    (if-let [errors (auth/validate-recovery-token path-params)]
      (do
        (u/log ::choose-password :errors errors)
        (layout/render request "general-error.html"
                       {:errors errors}))
      (do
        (u/log ::choose-password)
        (layout/render request "new-password.html" 
                       {:details path-params
                        :errors (:errors flash)})))))

(defn reset-password
  "POST method. Receives a request from `choose-password` containing the new password and
   and `token` inside it's `params`. If the password is valid then updates the password
   to the user who requested the creation of the `token`, and then redirects to the
   login page. Otherise redirects to the `referer` in `headers`."
  [{:keys [params headers] :as request}]
  (u/with-context {:request (clean-request request :params :form-params)
                   :params (dissoc params :__anti-forgery-token :psw :pwd)
                   :session/key (get-session request)}
    (if-let [errors (auth/validate-reset-password params)]
      (do
        (u/log ::reset-password :errors errors)
        (-> (response/found (get headers "referer"))
            (assoc :flash {:errors errors})))
      (let [tx-password (auth/update-password params)
            tx-update-token (auth/update-recovery-token params)]
        (u/log ::reset-password
               :tx-password (distinct (map :e (:tx-data tx-password)))
               :tempids-password (:tempids tx-password)
               :tx-update-token (distinct (map :e (:tx-data tx-update-token)))
               :tempids-update-token (:tempids tx-update-token))
        (ring.util.response/redirect "/ingresa")))))

(defn id-prueba [request]
  (layout/render request "id-upload.html"))

(defn general-template
  "GET request. Simple generic redirects to the specified html"
  [html request]
  (do
    (u/log ::general-template :html html
           :request (clean-request request)
           :session/key (get-session request))
    (layout/render request html)))

(defn onboarding-routes []
  [ "/user" 
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ;; onboarding
   ["/welcome" {:get register-page}]
   ["/validate-register" {:post validate-register}]
   ;; reset password
   ["/forgot" {:get recovery-email-address
               :post create-recovery-token}]
   ["/reset/:token" {:get choose-password}]
   ["/reset" {:post reset-password}]

   ;; validate email
   ["/wait" {:get validate-wait}]
   ["/validate-email/:token" {:get validate-email}]
   ["/resend" {:post resend-validate-email}]

   ;; general
   ["/error" {:get (partial general-template "general-error.html")}]
   ["/instructions" {:get (partial general-template "recover-email.html")}]

   ;; id
   ["/id-upload" {:get id-upload
                  :post validate-id}]
   ["/id-prueba" {:get id-prueba}]])
