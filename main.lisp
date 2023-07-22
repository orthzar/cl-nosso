;;;;main.lisp
;;;
;;; Nosso is a Nostr client library written in Common Lisp.
;;; This is intended to provide the essentials for interacting with Nostr relays and storing events.
;;; Ideally, this would be used to make a full-featured Nostr client.

(defpackage "cl-nosso")
(in-package :cl-nosso)

;;;
;;; Dependencies:
;;;

(ql:quickload :websocket-driver-client) ;; websocket client             https://github.com/fukamachi/websocket-driver
(ql:quickload :jonathan)                ;; JSON                         https://github.com/Rudolph-Miller/jonathan
(ql:quickload :lambdalite)              ;; data storage                 https://github.com/Wukix/LambdaLite
(ql:quickload :ironclad)                ;; cryptography                 https://github.com/sharplispers/ironclad/
(ql:quickload :flexi-streams)           ;; string-to-octet conversion   https://github.com/edicl/flexi-streams

(defvar *known-relays*
  '("wss://nostr-pub.wellorder.net"
    "wss://nostr.bitcoiner.social"
    "wss://nostr-relay.wlvs.space"
    "wss://nostr-relay.untethr.me"
    "wss://relay.damus.io"
    "wss://nostr.delo.software"
    "wss://relay.nostr.info"
    "wss://nostr-relay.freeberty.net"
    "wss://nostr.rocks"
    "wss://relayer.fiatjaf.com"
    "wss://nostr.drss.io"
    "wss://nostr.unknown.place"
    "wss://nostr.openchain.fr"
    "wss://rsslay.fiatjaf.com"
    "wss://freedom-relay.herokuapp.com/ws"
    "wss://nostr.onsats.org"
    "wss://nostr-verified.wellorder.net"
    "wss://relay.minds.com/nostr/v1/ws"))

;;;
;;; Storage of Nostr Events - using Lambdalite
;;;

(defun initialize-the-event-db (path) ;; e.g. "~/cl-nostr-db/"
  (lambdalite:load-db :path path))

(defun store-a-nostr-event (nostr-event)
  ;; Put the event into LambdaLite
  (lambdalite:insert
  ;; Parse the JSON into a Lisp property list
   (jonathan:parse nostr-event)))

;;;
;;; Cryptography - using Ironclad and Flexichain
;;;

;; WARNING / FIXME: This code stores private keys in the clear.
;; We need to encrypt them, but perhaps the entire database.
;; Until code for that is added, don't use this library for anything serious!
(defun make-key-pair ()
  (let
      ((key-pair (multiple-value-bind (private-key public-key)
                     (ironclad:generate-key-pair :secp256k1)
                   (list private-key public-key))))
    ;;(:/PUBLIC-KEY (:Y #(4 144 ... 151 30))
    ;; :/PRIVATE-KEY (:X #(75 36 ... 208 44)
    ;;                :Y #(4 144 ... 151 30)))
    (list
     ':/public-key
     (ironclad:destructure-public-key  (second key-pair))
     ':/private-key
     (ironclad:destructure-private-key (first key-pair)))))

(defun store-key-pair (key-pair)
  (lambdalite:insert :key-pairs key-pair))

(defun make-store-key-pair ()
  (store-key-pair (make-key-pair)))

(defun sha256 (string)
  "Given a string, return a hash of that string"
  ;; Lastly, we convert the byte-array into a string of hexadecimal characters and return that.
  (ironclad:byte-array-to-hex-string
   ;; Second, we hash the octets into a byte array.
   (ironclad:digest-sequence :sha256
    ;; First, we convert STRING to octets an array of octets, because that's what DIGEST-SEQUENCE requires for input.
    (flexi-streams:string-to-octets string))))

;; (ironclad:sign-message key message &key start end &allow-other-keys)
(defun sign-content (pub-key nostr-event-id)
  (ironclad:sign-message :secp256k1) )

;; (ironclad:verify-signature key message signature &key start end &allow-other-keys)
(defun verify-signature () )


;;;
;;; JSON - using Jonathan
;;;

(defun construct-a-nostr-event (pubkey created-at kind tags content sig)
  (let ((event-data (jonathan:with-array
                        (write-item 0)
                      (write-item pubkey)
                      (write-item created-at)
                      (write-item kinds)
                      (write-item tags)
                      (write-item content))))
    (jonathan:to-json
     :id (sha256 event-data)
     :pubkey pubkey
     :created_at created-at
     :kind kind
     :tags tags
     :content content
     :sig sig)))

;;;
;;; WebSocket - using WSD
;;;

;; Each websocket connection is an object, which we store as a value in a hash-table, the key being the relay's URL.
(defvar *websocket-connections* (make-hash-table :test 'equal))

(defun reference-a-websocket-connection (relay-url)
  (gethash relay-url *websocket-connections*))

(defun create-a-websocket-connection (relay-url)
  ;; We create an entry in the above hash-table, the key of which is the relay's URL.
  (let ((connection (reference-a-websocket-connection relay-url)))
    ;; We create a websocket client object, which connects to the relay, and then write that client object as the value for the above key.
    (setf connection (wsd:start-connection relay-url))
    ;; We tell our new websocket connection what to do when it receives a message from a relay.
    (wsd:on :message connection
            (lambda (message)
              (store-a-nostr-event message)))))

(defun destroy-a-websocket-connection (relay-url)
  (wsd:close-connection (gethash relay-url *websocket-connection*))
  (remhash relay-url *websocket-connection*))

(defun send-a-nostr-event-to-a-relay (nostr-event relay-url)
  (let ((connection (reference-a-websocket-connection relay-url)))
    (wsd:send-text connection nostr-event)))

