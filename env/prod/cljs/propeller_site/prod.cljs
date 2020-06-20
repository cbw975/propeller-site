(ns propeller-site.prod
  (:require
    [propeller-site.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
