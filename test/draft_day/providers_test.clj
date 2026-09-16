(ns draft-day.providers-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [draft-day.providers :as providers]))

(def ^:private sleeper-creds {:username "rockchalkjay"})
(def ^:private espn-creds
  {:swid    "{1A2B3C4D-5E6F-4A8B-9C0D-1E2F3A4B5C6D}"
   :espn-s2 (apply str (repeat 300 "A"))})

(deftest a-provider-may-arrive-as-a-string
  (is (= :espn (providers/provider-key "espn")))
  (is (= :espn (providers/provider-key :espn)))
  (is (nil? (providers/provider-key "yahoo"))
      "an unregistered host must not resolve, or a typo reaches a dispatcher")
  (is (nil? (providers/provider-key nil)))
  (is (nil? (providers/provider-key ""))))

(deftest an-unknown-provider-is-refused-rather-than-defaulted
  (is (some? (providers/credential-errors "yahoo" {:username "x"}))
      "defaulting a typo'd host looks up the wrong account and reports the wrong problem")
  (is (some? (providers/league-id-error "yahoo" "123")))
  (is (some? (providers/credential-errors nil sleeper-creds))))

(deftest a-username-may-carry-dots-but-never-a-traversal
  (is (nil? (providers/credential-errors :sleeper {:username "jay.h-2"})))
  (is (some? (providers/credential-errors :sleeper {:username "../../etc"}))
      "the pattern admits dots, so `..` needs its own clause or it reaches a URL path")
  (is (some? (providers/credential-errors :sleeper {:username "a..b"}))))

(deftest a-required-field-must-be-present
  (is (some? (providers/credential-errors :sleeper {})))
  (is (some? (providers/credential-errors :sleeper {:username "   "}))
      "whitespace is absence; the field is trimmed before it is judged")
  (is (some? (providers/credential-errors :espn (dissoc espn-creds :espn-s2))))
  (is (nil? (providers/credential-errors :espn espn-creds))))

(deftest a-swid-is-accepted-with-or-without-its-braces
  (is (nil? (providers/credential-errors
             :espn (assoc espn-creds :swid "1A2B3C4D-5E6F-4A8B-9C0D-1E2F3A4B5C6D"))))
  (is (some? (providers/credential-errors :espn (assoc espn-creds :swid "not-a-guid")))))

(deftest an-unnamed-credential-field-is-refused
  (is (some? (providers/credential-errors :sleeper (assoc sleeper-creds :token "x")))
      "a provider that one day reads an unnamed key would read input nothing validated"))

(deftest a-credential-error-never-echoes-the-value
  (let [secret "SUPERSECRETsentinel"
        {:keys [error]} (providers/credential-errors :espn {:swid secret :espn-s2 secret})]
    (is (some? error))
    (is (not (str/includes? error secret))
        "the message reaches the browser, and an echoed cookie is the leak redact exists for")))

(deftest redact-masks-every-secret-and-nothing-else
  (let [r (providers/redact :espn espn-creds)]
    (is (= providers/redacted (:swid r)))
    (is (= providers/redacted (:espn-s2 r))))
  (is (= sleeper-creds (providers/redact :sleeper sleeper-creds))
      "masking a public username would make every diagnosable failure unreadable")
  (testing "every secret field of every provider is covered"
    (doseq [p (providers/providers)
            :let [creds (zipmap (providers/field-keys p) (repeat "sentinel"))
                  r     (providers/redact p creds)]
            k (providers/secret-keys p)]
      (is (= providers/redacted (get r k)) (str p " " k)))))

(deftest every-provider-can-actually-be-connected
  (doseq [p (providers/providers)]
    (is (seq (providers/fields p))
        (str (name p) " has no fields — its Connect button would submit nothing"))
    (is (some :required? (providers/fields p)) (str (name p) " requires nothing"))
    (is (some? (providers/league-id-label p)) (str (name p) " has no league-id label"))
    (is (some? (get-in providers/catalog [p :league-id :pattern]))
        (str (name p) " has no league-id pattern — the id reaches a URL path unguarded"))))

(deftest a-league-id-must-be-safe-in-a-url-path
  (is (nil? (providers/league-id-error :sleeper "987654")))
  (is (some? (providers/league-id-error :sleeper "")))
  (is (some? (providers/league-id-error :sleeper "../secrets")))
  (is (nil? (providers/league-id-error :espn "12345"))))
