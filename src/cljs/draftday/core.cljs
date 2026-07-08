(ns draftday.core
  "re-frame entry point. Phase 1 scaffold — proves the CLJS/re-frame toolchain
  builds and mounts; real views arrive in Phase 5."
  (:require [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]))

(rf/reg-event-db
 :init
 (fn [_ _]
   {:msg "scaffold up — re-frame is running"}))

(rf/reg-sub
 :msg
 (fn [db _]
   (:msg db)))

(defn app-view []
  [:div {:style {:padding "2rem" :font-family "system-ui, sans-serif"}}
   [:h1 "🏈 Draft Day"]
   [:p @(rf/subscribe [:msg])]])

(defonce root (rdomc/create-root (.getElementById js/document "app")))

(defn ^:export init []
  (rf/dispatch-sync [:init])
  (rdomc/render root [app-view]))
