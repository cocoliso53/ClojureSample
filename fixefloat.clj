(ns website.fixedfloat
  (:require
   [buddy.core.mac :as mac]
   [buddy.core.codecs :as codecs]
   [website.db.cloud-core :as dat]
   [clj-http.client :as client]
   [clojure.data.json :as json]))

(defn helper-query-string
  "Gets a map with keys
   `:fromCurrency` - string
   `:fromQty` - string
   `:toCurrency` - string
   and returns a query string required for fixedfloat API"
  [{:keys [fromCurrency fromQty toCurrency]}]
  (format "fromCurrency=%s&fromQty=%s&toCurrency=%s&type=float"
          fromCurrency fromQty toCurrency))

(defn helper-emergency-string
  "Given a map `m` with the needed data creates the query string needed for
   set-emergency for fixedfloat api, refund address is hardcoded"
  [{:keys [id token]}]
  (format "id=%s&token=%s&choice=REFUND&address=0xd9ee72639C12B7e5BBe355E667fDb69Deb6Ca89D"
          id token))

(defn sign-query-string
  "Signs the `query-string` with the secret-key from fixedfloat"
  [query-string]
  (-> (mac/hash query-string
                {:key secret
                 :alg :hmac+sha256})
      (codecs/bytes->hex)))

(defn quote-order
  "Takes a map with keys
   `:fromCurrency` - string
   `:fromQty` - string
   `:toCurrency` - string
   make a post request and returns the http response from the fixedfloat API"
  [m]
  (let [query-string (helper-query-string m)]
    (client/post "https://fixedfloat.com/api/v1/getPrice"
                 {:headers {"X-API-KEY" api-key
                            "X-API-SIGN" (sign-query-string query-string)}
                  :content-type "application/x-www-form-urlencoded"
                  :body query-string})))

(defn create-order-params
  "Takes a map with order data comming from the frontend and return the
   data needed to place an order on fixedfloat"
  [{:keys [from_currency inter_amount to_currency to_address from_amount network]}]
  (if (= from_currency "MXN")
    {:fromCurrency "BUSDBSC" :fromQty (-> inter_amount
                                          clojure.edn/read-string
                                          (* 0.995)
                                          str)
     :toCurrency (if (= "BTC" network)
                   "BTC"
                   (get currency-key to_currency))  :toAddress to_address}
    (if (= to_currency "MXN")
      {:fromCurrency (if (= "BTC" network)
                       "BTC"
                       (get currency-key from_currency)) :fromQty from_amount
       :toCurrency "BUSDBSC" :toAddress "0xd9ee72639C12B7e5BBe355E667fDb69Deb6Ca89D"})))

(defn create-order
  "Takes a map with keys 
   `:fromCurrency` - string
   `:fromQty` - string
   `:toCurrency` - string
   `:toAddress` - string
   places an order on fixedfloat. Check fixedfloat docs for further info"
  [m]
  (let [query-string (str (helper-query-string m)
                          "&toAddress=" (:toAddress m))]
    (client/post "https://fixedfloat.com/api/v1/createOrder"
                 {:headers {"X-API-KEY" api-key
                            "X-API-SIGN" (sign-query-string query-string)}
                  :content-type "application/x-www-form-urlencoded"
                  :body query-string})))

(defn get-order
  "Takes a map with keys
   `:id` - string
   `:token` - string
   Returns the details for the specified order on fixedfloat"
  [{:keys [id token]}]
  (let [get-string (str "id=" id "&token=" token)]
    (->
     (client/get (str "https://fixedfloat.com/api/v1/getOrder?" get-string)
                 {:headers {"X-API-KEY" api-key
                            "X-API-SIGN" (sign-query-string get-string)}
                  :content-type "application/x-www-form-urlencoded"})
     :body)))


(defn extract-relevant
  "Takes a response from fixedfloat api after an order has been placed
   and returns only the relevant data"
  [resp-ff]
  (let [{:keys [code msg data]} (-> resp-ff
                                    :body
                                    (json/read-str :key-fn keyword))
        from (:from data)
        to (:to data)]
    {:code code
     :msg msg
     :data {:token (:token data)
            :id (:id data)
            :from_address_swap (:address from)
            :to_amount_swap (:amount to)}}))

(defn set-emergency
  "Once an order has succedfully been placed on fixedfloat, 
   place emergency settings for the order. Input must be the
   responce body from `create-order` already with keywords"
  [{:keys [data]}]
  (let [emergency-string (helper-emergency-string data)]
    (client/get (str "https://fixedfloat.com/api/v1/setEmergency?"
                     emergency-string)
                {:headers {"X-API-KEY" api-key
                            "X-API-SIGN" (sign-query-string emergency-string)}
                 :content-type "application/x-www-form-urlencoded"})))

(defn relevant-data-swap
  "Given a map with data from an order, places the order on
   fixedfloat and returns the relevant data"
  [{:keys [network] :as m}]
  (if (.contains ff-nw network)
    (-> m
        create-order-params
        create-order
        extract-relevant)))

(defn valid-fixedfloat-order?
  "Check if the order was placed correctly"
  [{:keys [code msg]}]
  (if (and (= code 0)
           (= msg "OK"))
    {:valid? true}
    {:valid? false :msg msg}))

(defn final-tx-hash
  "For a map with `:id` and `:token` keys, returns the
   from tx-hash and to tx-hash once the order has been completed as well as the 
   final amounts"
  [m]
  (let [order (json/read-str (get-order m) :key-fn keyword)]
    {:to_hash (-> order :data :to :tx :id)
     :to_amount (-> order :data :to :tx :amount Float/parseFloat)
     :from_hash (-> order :data :from :tx :id)
     :from_amount (-> order :data :from :tx :amount Float/parseFloat)}))
