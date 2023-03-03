(ns website.operations
  (:require
   [clojure.string :as string]
   [website.db.cloud-core :as dat]
   [website.fixedfloat :as ff]
   [website.changenow :as cn]
   [com.brunobonacci.mulog :as u]
   [datomic.client.api :as d]
   [website.eth :as eth]
   [clj-uuid :as uuid]))


;;; --- Order ops --- ;;;

(defn helper-new-order
  [{:keys [from_currency to_currency concepto from_address uuid network]}]
  (let [uuid-s (.toString uuid)]
    (if (= from_currency "MXN")
      [[:db/add uuid-s :order.from/network :network/SPEI]
       [:db/add uuid-s :order.from/address [:wallet/pseudonym "SUARMI-ALBO"]]
       [:db/add uuid-s :order.from/concepto concepto]
       [:db/add uuid-s :order.to/network (keyword "network" network)]]
      [[:db/add uuid-s :order.to/network :network/SPEI]
       [:db/add uuid-s :order.from/address [:wallet/address from_address]]
       [:db/add uuid-s :order.from/network (keyword "network" network)]])))


(defn new-order-coll
  "Given the initial data from an order returns a collection
   to be transacted to  create an order on the DB"
  [{:keys [from_currency from_amount from_address inter_amount network
           to_currency to_amount to_address user uuid to_memo from_memo platform] :as m}] ; pending
  (let [uuid-s (.toString uuid)
        inter-currency (get nv-coins network)
        base [[:db/add uuid-s :order/uuid uuid]
              [:db/add uuid-s :order/status :order.status/waiting-funding]
              [:db/add uuid-s :order/cdate (java.util.Date.)]
              [:db/add uuid-s :order.from/currency (curs->curkw from_currency)]
              [:db/add uuid-s :order.from/amount (Float/parseFloat from_amount)]
              [:db/add uuid-s :order.to/currency (curs->curkw to_currency)]
              [:db/add uuid-s :order.to/amount (Float/parseFloat to_amount)]
              [:db/add uuid-s :order.to/address [:wallet/address to_address]]
              [:db/add uuid-s :order.inter/currency inter-currency]
              [:db/add uuid-s :order.inter/amount (Float/parseFloat inter_amount)]
              [:db/add uuid-s :order/user [:user/email user]]]]
    (reduce into (list
                  base
                  (helper-new-order m)
                  (if (nil? platform) [] [[:db/add uuid-s :order/platform
                                          [:user/email platform]]])
                  (if (nil? from_memo) [] [[:db/add uuid-s :order.from/memo
                                            [:memo/id from_memo]]])
                  (if (nil? to_memo) [] [[:db/add uuid-s :order.to/memo
                                          [:memo/id to_memo]]])))))

;;; ---  Swap ops --- ;;;

(defn new-swap-coll
  "Takes a map with the data needed to transact a swap entity,
   at this point the order is already placed in the corresponding platform and
   a `from_address_swap` is needed, here is where the funds will
   be sent (either by the user or us) to start the swap operation"
  [{:keys [id token from_currency to_currency inter_amount to_address to_memo from_memo_swap
           from_amount to_address uuid from_address_swap to_amount_swap network]}]
  (let [uuid-swap (uuid/v1)
        uuid-s (str uuid-swap)]
    (reduce into (list
                  [[:db/add uuid-s :swap/uuid uuid-swap]
                   [:db/add uuid-s :swap/status :swap.status/waiting-funding]
                   [:db/add uuid-s :swap/cdate (java.util.Date.)]
                   [:db/add uuid-s :swap/tx-id id]
                   [:db/add uuid-s :swap/orders (str uuid)]
                   [:db/add uuid-s :swap.from/address [:wallet/address from_address_swap]]
                   [:db/add uuid-s :swap.to/amount (Float/parseFloat to_amount_swap)]]
                  (if (nil? token)
                    [] [[:db/add uuid-s :swap/tx-memo token]])
                  (if (nil? to_memo)
                    [] [[:db/add uuid-s :swap.to/memo [:memo/id to_memo]]])
                  (if (nil? from_memo_swap)
                    [] [[:db/add uuid-s :swap.from/memo [:memo/id from_memo_swap]]])
                  (if (= from_currency "MXN")
                    [[:db/add uuid-s :swap.from/currency :currency/BUSD]
                     [:db/add uuid-s :swap.from/amount (Float/parseFloat inter_amount)]
                     [:db/add uuid-s :swap.from/network :network/BSC]
                     [:db/add uuid-s :swap.to/currency (keyword "currency" to_currency)]
                     [:db/add uuid-s :swap.to/address [:wallet/address to_address]]
                     [:db/add uuid-s :swap.to/network (keyword "network" network)]]
                    [[:db/add uuid-s :swap.from/currency (keyword "currency" from_currency)]
                     [:db/add uuid-s :swap.from/amount (Float/parseFloat from_amount)]
                     [:db/add uuid-s :swap.from/network (keyword "network" network)]
                     [:db/add uuid-s :swap.to/currency :currency/BUSD]
                     [:db/add uuid-s :swap.to/address
                      [:wallet/pseudonym "CUAU-SUARMI-HOT-EVM"]] ; WHERE BUSD will be deposited
                     [:db/add uuid-s :swap.to/network :network/BSC]])))))

;;; --- Full ---;;;

(defn helper-complete-data
  "Helper function, depending on conditions returns full map"
  [{:keys [from_currency user network] :as m}]
  (let [uuid (uuid/v1)]
    (if (= from_currency "MXN")
      (assoc m :concepto (spei-concepto uuid)
               :uuid uuid
               :from_address "646180204200011681")
      (assoc m :uuid uuid :from_address (if (.contains nv-nw network)
                                          (:address (eth/user-wallet user))
                                          (:from_address_swap m))
                          :from_memo (if (-> m :from_memo_swap nil?)
                                       nil
                                       (:from_memo_swap m))))))

(defn complete-data
  "For a map comming from the front end containing the basic info
   of an order, returns an extended map with all the info needed
   to create the collections to be transacted in the db. In case the data
   was not correct (in particular swap orders) then returns a map
   with an `:msg` key displaying the error. The map should cointain:

    `from_currency` - string
    `from_amount` - string
    `inter_amount` - string
    `to_currency` - string
    `to_amount` - string
    `to_address` - string
    `to_memo` - string (optional)
    `user` - string (email of user)
    `network` - blockchain involved in the order, ie antagonist of SPEI 
     and returns a collection ready to be transacted into the db
    `platform` - string (optional)
  "
  [{:keys [from_currency to_currency to_address user network] :as m}]
  (let [_ (new-wallet-memo m)]
    (if (.contains nv-nw network)
      (helper-complete-data m)
      (let [relevant-swap (future (relevant-data-swap m))
            validity (valid-swap-order @relevant-swap)]
        (if (:valid? validity)
          (let [data-swap (:data @relevant-swap)
                order-n-swap (merge m data-swap)
                _ (new-wallet-memo {:to_address (:from_address_swap data-swap)
                                    :to_memo (:from_memo_swap data-swap)
                                    :user user})]
            (helper-complete-data order-n-swap))
          validity)))))


(defn complete-colls
  "Gets a map with keywords:
   `from_currency` - string
   `from_amount` - string
   `from_address` - string
   `from_memo` - string (optional)
   `concepto` - string (only when `from_currency` is MXN)
   `inter_amount` - string
   `to_currency` - string
   `to_amount` - string
   `to_address` - string
   `to_memo` - string (optional)
   `user` - string (email of user)
   `uuid` - uuid, this one will belong to an order
   `id` - string  (only when swap is involved)
   `token` - string (only when BTC is involved)
   `from_address_swap` - string (only when swap is involved)
   `from_memo_swap` - string (only when swap is involved and only for certain networks)
   `to_amount_swap` - string (only when swap is involved)
   `platform` - string (only when order is being placed through 3rd party)
   `network` - blockchain involved in the order, ie antagonist of SPEI 
    and returns a collection ready to be transacted into the db"
  [{:keys [from_currency to_currency network] :as m}]
  (let [order-coll (new-order-coll  m)]
    (if (.contains nv-nw network)
      order-coll
      (into order-coll (new-swap-coll m)))))


(defn transact-order
  "Transacts a new order in the db. Checks if the wallets addresses exists,
   if not then it's created. Once it's done trasacts the order data. The
   `m` map comes directly from the frontend and adding the user's email"
  [m]
  (let [complete-map (complete-data m)]
    (if (nil? (:msg complete-map))
      (try
        (let [complete-coll (complete-colls complete-map)
              tx-order (dat/make-db-transaction dat/conn complete-coll)]
          (do
            (u/log ::transact-order :tempids-order (:tempids tx-order)
                   :tx-data-order (distinct (map :e (:tx-data tx-order))))
            {:tx-data-order (distinct (map :e (:tx-data tx-order)))
             :tempids-order (:tempids tx-order)
             :address (:from_address complete-map)
             :concepto (:concepto complete-map)
             :uuid (:uuid complete-map)
             :from_address_swap (:from_address_swap complete-map)
             :from_memo_swap (:from_memo_swap complete-map)}))
        (catch Exception e
          (do
            (u/log ::transact-order :error "Error en tx data"
                   :exception e)
            {:msg "Error Interno"})))
      (do
        (u/log ::transact-order :error "Error en fixedfloat"
               :msg (:msg complete-map))
        complete-map))))

(defn test-transact-order
  "TEST FUNCTION DOESN'T ACTUALLY TRANSACTS ANYTHIG

   Transacts a new order in the db. Checks if the wallets addresses exists,
   if not then it's created. Once it's done trasacts the order data. The
   `m` map comes directly from the frontend and appending the user's email"
  [m]
  (let [complete-map (complete-data m)]
    (if (nil? (:msg complete-map))
      (try
        (let [complete-coll (complete-colls complete-map)
              tx-order (d/with (d/with-db dat/conn) {:tx-data complete-coll})]
          (do
            (u/log ::test-transact-order :tempids-order (:tempids tx-order)
                   :tx-data-order (distinct (map :e (:tx-data tx-order))))
            {:tx-data-order (distinct (map :e (:tx-data tx-order)))
             :tempids-order (:tempids tx-order)
             :address (:from_address complete-map)
             :concepto (:concepto complete-map)
             :uuid (:uuid complete-map)
             :from_address_swap (:from_address_swap complete-map)
             :from_memo_swap (:from_memo_swap complete-map)}))
        (catch Exception e
          (do
            (u/log ::test-transact-order :error "Error en tx data"
                   :exception e)
            {:msg "Error Interno"})))
      (do
        (u/log ::test-transact-order :error "Error en fixedfloat"
               :msg (:msg complete-map))
        complete-map))))
                   

;;; --- Orders and swap info (might move to namespace of it's own)  --- ;;;

(defn helper-order-history
  "Takes a key and a function and returns a transducer (function). In escence this is a function
   that returns  functions. It just helps to keep the code cleaner"
  [k f]
  (map (fn [m] (update m k f))))

(defn order-history
  "Meant to be used in home.clj, returns coll with the date, receiver, amount and status
   details for each transfer made by a given user"
  [user-email]
  (let [sort-reverse (comp reverse (partial sort-by :date))
        t-c (dat/get-user-orders-detail (dat/get-db dat/conn) user-email)
        t-m (map (partial zipmap [:id :date :amount :currency :address :status]) t-c)
        xf (comp
            (helper-order-history :id str)
            (helper-order-history :date #(.format (java.text.SimpleDateFormat. "yyyy-MM-dd hh:mm:ss")
                                                  %))
            (helper-order-history :currency name)
            (helper-order-history :status name))]
    (sort-reverse (vec (eduction xf t-m)))))

(defn order-detail
  "For a given uuid belonging to an order returns it's details"
  [uuid-s]
  (let [t-c (dat/get-order-info (dat/get-db dat/conn) uuid-s)
        t-m (map (partial zipmap [:user :status :date
                                  :from_amount :from_currency :from_address
                                  :to_amount :to_currency :to_address :concepto
                                  :from_memo :to_memo :from_network :to_network]) t-c)
        xf (comp
            (helper-order-history :date str)
            (helper-order-history :to_currency name)
            (helper-order-history :from_currency name)
            (helper-order-history :from_network name)
            (helper-order-history :status name))]
    ((comp first eduction) xf t-m)))

(defn order-tx-hash
  "For a given `uuid` belonging to an order, returns the `to_network` and
   tx-hash where the funds are sent to the users address"
  [uuid-s]
  (let [send-info (dat/get-order-send-hash (dat/get-db dat/conn) uuid-s)]
    (if (empty? send-info)
      (let [swap-info (dat/get-order-swap-hash (dat/get-db dat/conn) uuid-s)]
        (if (or (empty? swap-info)
                (= (last (first swap-info)) :network/BSC))
          {:tx-hash "NA"}
          {:tx-hash (ffirst swap-info)
           :network (name (last (first swap-info)))}))
      {:tx-hash (ffirst send-info)
       :network (name (last (first send-info)))})))
