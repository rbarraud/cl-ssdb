(in-package :cl-user)

(defpackage cl-ssdb-test
  (:use :cl :prove))

(in-package :cl-ssdb-test)

(defvar *ssdb-auth* "8AgYA/ougM9DCvtS9l172ddhb+v/nIAPvVpJjMdzHz0VnkB6Ji3qC904O1XuOj+6ZD9cqJq3xgsPQRUzGDA9YZfI8z/hA5G6fIxnB9g/VLUo4GrEHRJ5bMV3h8MknaHreQQ7wjxuzgkNLfrxsev6ZULCXWUizbNU3hthMIQjjtS6L+yRnOcKBH+f7IsCyZJsikCmNSFpyN2DaKqFnqTJphueuodkKAqK+N3iCjsrhuucvW3GkZOpXkBILicSDZ36dXQnKDQPctr8w4RTLzbg0lb7b5qcO134A6aVRl4WrBScwRf6HNB+r0p+8zeWAcmdtIQbYwY0wh7A6tS0mWOpTTD5BQCiqL+WwZkL3i2K3wAK0tx3WI9g8I4k8JBwxdLwi0I1UW2dF5oMpk4KppB6nqckya4ybkyGAFw6op3IhAa6m/jvWZUaZrN8im/tlcCoDFfQCywWx4V06eqa35221c+9DbpXfFvJcFxZ6xKJkf0I34SIarLI+pVqm/k3Q0Dc")

(defmacro with-test-db (&body body)
  `(ssdb:with-connection ()
     (ssdb:auth *ssdb-auth*)
     (ssdb:flushdb)
     (unwind-protect
          (progn ,@body)
       (ssdb:flushdb))))

(plan nil)

(subtest "Testing connection"
  (is nil (ssdb:disconnect))
  (let ((connection (ssdb:connect)))
    (is t (ssdb::connection-open-p connection)))
  (is-error (ssdb:connect) 'ssdb:ssdb-error)
  (is nil (ssdb:disconnect))
  (ssdb:with-connection ()
    (is-error (ssdb:ping) 'ssdb:ssdb-error-reply)
    (is "ok" (ssdb:auth *ssdb-auth*))
    (is "ok" (ssdb:ping))))

;;; test Server commands
(subtest "Testing Server Commands"
  (with-test-db
    (is "ok" (ssdb:ping))
    (is "ok" (ssdb:clear_binlog))
    (is "ok" (ssdb:compact))
    (is-type (ssdb:dbsize) 'integer)
    (is-type (ssdb:info) 'string)))

;; test flushdb
(subtest "Testing flushdb"
  (ssdb:with-connection ()
    (ssdb:auth *ssdb-auth*)
    (ssdb:flushdb)
    (is-values (ssdb:flushdb)
               '(0
                 ("kv" . 0)
                 ("hash" . 0)
                 ("zset" . 0)
                 ("list" . 0)))
    (is "ok" (ssdb:set "key1" "value1"))
    (is t (ssdb:hset "hkey1" "field1" "value1"))
    (is t (ssdb:hset "hkey2" "field1" "value1"))
    (is t (ssdb:zset "zkey1" "field1" 1))
    (is t (ssdb:zset "zkey2" "field1" 1))
    (is t (ssdb:zset "zkey3" "field1" 1))
    (is 1 (ssdb:qpush "lkey1" "item1"))
    (is 1 (ssdb:qpush "lkey2" "item1"))
    (is 1 (ssdb:qpush "lkey3" "item1"))
    (is 1 (ssdb:qpush "lkey4" "item1"))
    (is-values (ssdb:flushdb)
               '(10
                 ("kv"   . 1)
                 ("hash" . 2)
                 ("zset" . 3)
                 ("list" . 4)))))

;; test kv commands
(subtest "Testing kv commands"
  (with-test-db
    (subtest "Testing set get exists del"
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:get "key1")             "value1")
      (is (ssdb:set "key1" "value2")    "ok")
      (is (ssdb:get "key1")             "value2")
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:get "key1")             "value1")
      (is (ssdb:exists "key1")          t)
      (is (ssdb:del "key1")             "ok")
      (is (ssdb:exists "key1")          nil))
    (subtest "Testing getset 1"
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:get "key1")             "value1")
      (is (ssdb:getset "key1" "value2") "value1")
      (is (ssdb:get "key1")             "value2")
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing getset 2"
      (is (ssdb:getset "not-exists-key" "value2") nil)
      (is (ssdb:exists "not-exists-key") t)
      (is (ssdb:get "not-exists-key")   "value2")
      (is (ssdb:del "not-exists-key")   "ok"))
    (subtest "Testing multi_set multi_get multi_del"
      (ssdb:del "key1")
      (ssdb:del "key2")
      (ssdb:del "key3")
      (is (ssdb:multi_set "key1" "value1" "key2" "value2" "key3" "value3") 3)
      (is (ssdb:exists "key1")          t)
      (is (ssdb:exists "key2")          t)
      (is (ssdb:exists "key3")          t)
      (is (ssdb:get "key1")             "value1")
      (is (ssdb:get "key2")             "value2")
      (is (ssdb:get "key3")             "value3")
      (is (ssdb:multi_get "key1" "key2" "key3")
          '("key1" "value1" "key2" "value2" "key3" "value3")
          :test #'equal)
      (is (ssdb:multi_get "key1" "key2" "not-exists-key")
          '("key1" "value1" "key2" "value2")
          :test #'equal)
      (is (ssdb:multi_get "not-exists-key") nil)
      (is (ssdb:multi_del "key1" "key2" "key3") 3)
      (is (ssdb:exists "key1")          nil)
      (is (ssdb:exists "key2")          nil)
      (is (ssdb:exists "key3")          nil)
      (is (ssdb:multi_del "not-exists-key") 1)
      (is (ssdb:multi_del "not-exists-key" "not-exists-key2") 2))
    (subtest "Testing incr"
      (is (ssdb:set "key1" 1)           "ok")
      (is (ssdb:get "key1")             "1")
      (is (ssdb:incr "key1")            2)
      (is (ssdb:incr "key1" 2)          4)
      (is (ssdb:set "key1" "value1")    "ok")
      (is-error (ssdb:incr "key1")      'ssdb:ssdb-error-reply))
    (subtest "Testing ttl"
      (is (ssdb:ttl "not-exists-key")   -1)
      (is (ssdb:setx "key1" "value1" 1) "ok")
      (isnt (ssdb:ttl "key1")           -1)
      (is (ssdb:exists "key1")          t)
      (sleep 1.2)
      (is (ssdb:exists "key1")          nil))
    (subtest "Testing expire 1"
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:expire "key1" 2)        t)
      (is (ssdb:exists "key1")          t)
      (sleep 2.1)
      (is (ssdb:exists "key1")          nil))
    (subtest "Testing expire 2"
      (is (ssdb:setx "key1" "value1" 1) "ok")
      ;; update ttl with ssdb:expire
      (is (ssdb:expire "key1" 2)        t)
      (is (ssdb:exists "key1")          t)
      (sleep 1.1)
      (isnt (ssdb:ttl "key1")           -1)
      (is (ssdb:exists "key1")          t)
      (sleep 1)
      (is (ssdb:ttl "key1")             -1)
      (is (ssdb:exists "key1")          nil))
    (subtest "Testing setnx"
      (is (ssdb:del "key1")             "ok")
      (is (ssdb:setnx "key1" "value2")  t)
      (is (ssdb:setnx "key1" "value2")  nil)
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing setbit getbit bitcount countbit 1"
      (is (ssdb:setbit "key1" 0 1)      0)
      (is (ssdb:setbit "key1" 0 1)      1)
      (is (ssdb:getbit "key1" 0)        1)
      (is (ssdb:setbit "key1" 0 0)      1)
      (is (ssdb:getbit "key1" 0)        0)
      (is (ssdb:bitcount "key1")        0)
      (is (ssdb:setbit "key1" 0 1)      0)
      (is (ssdb:setbit "key1" 1 0)      0)
      (is (ssdb:setbit "key1" 2 1)      0)
      (is (ssdb:getbit "key1" 0)        1)
      (is (ssdb:getbit "key1" 1)        0)
      (is (ssdb:getbit "key1" 2)        1)
      (is (ssdb:bitcount "key1")        2)
      (is (ssdb:bitcount "key1" 0 1)    2)
      (is (ssdb:bitcount "key1" 1 2)    0)
      (is (ssdb:bitcount "key1" 0 2)    2)
      (is (ssdb:bitcount "key1" 0 3)    2)
      (is (ssdb:bitcount "key1" 0 4)    2)
      (is (ssdb:bitcount "not-exists-key") 0)
      (is (ssdb:countbit "not-exists-key") 0)
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing setbit getbit bitcount countbit 2"
      (is (ssdb:del "key1")             "ok")
      (is (ssdb:set "key1" "a")         "ok")
      (is (ssdb:getbit "key1" 0)        1)
      (is (ssdb:getbit "key1" 1)        0)
      (is (ssdb:getbit "key1" 2)        0)
      (is (ssdb:getbit "key1" 3)        0)
      (is (ssdb:getbit "key1" 4)        0)
      (is (ssdb:getbit "key1" 5)        1)
      (is (ssdb:getbit "key1" 6)        1)
      (is (ssdb:getbit "key1" 7)        0)
      (is (ssdb:setbit "key1" 1 1)      0)
      (is (ssdb:setbit "key1" 0 0)      1)
      (is (ssdb:get "key1")             "b")
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing setbit getbit bitcount countbit 3"
      (is (ssdb:del "key1")             "ok")
      (is (ssdb:set "key1" "ac")        "ok")
      (is (ssdb:getbit "key1" 0)        1)
      (is (ssdb:getbit "key1" 1)        0)
      (is (ssdb:getbit "key1" 2)        0)
      (is (ssdb:getbit "key1" 3)        0)
      (is (ssdb:getbit "key1" 4)        0)
      (is (ssdb:getbit "key1" 5)        1)
      (is (ssdb:getbit "key1" 6)        1)
      (is (ssdb:getbit "key1" 7)        0)
      (is (ssdb:getbit "key1" 8)        1)
      (is (ssdb:getbit "key1" 9)        1)
      (is (ssdb:getbit "key1" 10)       0)
      (is (ssdb:getbit "key1" 11)       0)
      (is (ssdb:getbit "key1" 12)       0)
      (is (ssdb:getbit "key1" 13)       1)
      (is (ssdb:getbit "key1" 14)       1)
      (is (ssdb:getbit "key1" 15)       0)
      (is (ssdb:bitcount "key1")        7)
      (is (ssdb:bitcount "key1" 0 1)    7) ;; index by bytes, not bits
      (is (ssdb:bitcount "key1" 1 2)    4) ;; index by bytes, not bits
      (is (ssdb:countbit "key1")        7)
      (is (ssdb:countbit "key1" 0 1)    3)
      (is (ssdb:countbit "key1" 1 1)    4)
      (is (ssdb:setbit "key1" 1 1)      0)
      (is (ssdb:get "key1")             "cc")
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing substr strlen"
      (is (ssdb:strlen "not-exists-key") 0)
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:strlen "key1")          6)
      (is (ssdb:substr "key1" 0 1)      "v")
      (is (ssdb:substr "key1" 0 2)      "va")
      (is (ssdb:substr "key1" 0 3)      "val")
      (is (ssdb:substr "key1" 1 3)      "alu")
      (is (ssdb:substr "key1" 0 7)      "value1")
      (is (ssdb:substr "key1" 0 8)      "value1")
      (is (ssdb:substr "key1" 1 7)      "alue1")
      (is (ssdb:del "key1")             "ok"))
    (subtest "Testing keys rkeys 1"
      (is (ssdb:keys "" "" -1)          nil)
      (is (ssdb:rkeys "" "" -1)         nil)
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:keys "" "" -1)          '("key1") :test #'equal)
      (is (ssdb:rkeys "" "" -1)         '("key1") :test #'equal)
      (is (ssdb:set "key2" "value2")    "ok")
      (is (ssdb:keys "" "" -1)          '("key1" "key2") :test #'equal)
      (is (ssdb:rkeys "" "" -1)         '("key2" "key1") :test #'equal)
      (is (ssdb:del "key1")             "ok")
      (is (ssdb:keys "" "" -1)          '("key2") :test #'equal)
      (is (ssdb:rkeys "" "" -1)         '("key2") :test #'equal)
      (is (ssdb:del "key2")             "ok")
      (is (ssdb:keys "" "" -1)          nil)
      (is (ssdb:rkeys "" "" -1)         nil))
    (subtest "Testing keys rkeys 2"
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:set "key2" "value1")    "ok")
      (is (ssdb:set "key3" "value1")    "ok")
      (is (ssdb:set "key4" "value1")    "ok")
      (is (ssdb:keys "" "" -1)
          '("key1" "key2" "key3" "key4")
          :test #'equal)
      (is (ssdb:rkeys "" "" -1)
          '("key4" "key3" "key2" "key1")
          :test #'equal)

      (is (ssdb:keys "" "key2" -1)      '("key1" "key2") :test #'equal)
      (is (ssdb:keys "key1" "key2" -1)  '("key2") :test #'equal)
      (is (ssdb:keys "key1" "key3" -1)  '("key2" "key3") :test #'equal)
      (is (ssdb:keys "" "" 2)           '("key1" "key2") :test #'equal)
      (is (ssdb:keys "" "" 3)           '("key1" "key2" "key3") :test #'equal)

      (is (ssdb:rkeys "" "key2" -1)     '("key4" "key3" "key2") :test #'equal)
      (is (ssdb:rkeys "key4" "key2" -1) '("key3" "key2") :test #'equal)
      (is (ssdb:rkeys "key4" "key3" -1) '("key3") :test #'equal)
      (is (ssdb:rkeys "" "" 2)          '("key4" "key3") :test #'equal)
      (is (ssdb:rkeys "" "" 3)          '("key4" "key3" "key2") :test #'equal)

      (is (ssdb:del "key1")             "ok")
      (is (ssdb:del "key2")             "ok")
      (is (ssdb:del "key3")             "ok")
      (is (ssdb:del "key4")             "ok")
      (is (ssdb:keys "" "" -1)          nil)
      (is (ssdb:rkeys "" "" -1)         nil))
    (subtest "Testing keys rkeys scan rscan 3"
      (is (ssdb:set "key1" "value1")    "ok")
      (is (ssdb:set "key2" "value1")    "ok")
      (is (ssdb:set "key3" "value1")    "ok")
      (is (ssdb:set "key4" "value1")    "ok")
      (is (ssdb:set "akey1" "value1")   "ok")
      (is (ssdb:set "akey2" "value1")   "ok")
      (is (ssdb:set "akey3" "value1")   "ok")
      (is (ssdb:set "akey4" "value1")   "ok")
      ;; Note: SSDB will sort keys by string< ,
      ;;   result: "akey1" "akey2" "akey3" "akey4" "key1" "key2" "key3" "key4"
      
      (is (ssdb:keys "key" "" -1)       '("key1" "key2" "key3" "key4") :test #'equal)
      (is (ssdb:rkeys "key" "" -1)      '("akey4" "akey3" "akey2" "akey1") :test #'equal)

      (is (ssdb:keys "" "key2" -1)
          '("akey1" "akey2" "akey3" "akey4" "key1" "key2")
          :test #'equal)
      (is (ssdb:keys "key1" "key2" -1)  '("key2") :test #'equal)
      (is (ssdb:keys "key1" "key3" -1)  '("key2" "key3") :test #'equal)
      (is (ssdb:keys "" "" 2)           '("akey1" "akey2") :test #'equal)
      (is (ssdb:keys "" "" 3)           '("akey1" "akey2" "akey3") :test #'equal)

      (is (ssdb:rkeys "" "key2" -1)     '("key4" "key3" "key2") :test #'equal)
      (is (ssdb:rkeys "key4" "key2" -1) '("key3" "key2") :test #'equal)
      (is (ssdb:rkeys "key4" "key3" -1) '("key3") :test #'equal)
      (is (ssdb:rkeys "" "" 2)          '("key4" "key3") :test #'equal)
      (is (ssdb:rkeys "" "" 3)          '("key4" "key3" "key2") :test #'equal)

      (is (ssdb:del "key1")             "ok")
      (is (ssdb:del "key2")             "ok")
      (is (ssdb:del "key3")             "ok")
      (is (ssdb:del "key4")             "ok")
      (is (ssdb:del "akey1")            "ok")
      (is (ssdb:del "akey2")            "ok")
      (is (ssdb:del "akey3")            "ok")
      (is (ssdb:del "akey4")            "ok")
      (is (ssdb:keys "" "" -1)          nil)
      (is (ssdb:rkeys "" "" -1)         nil))))

(subtest "Testing hashmap commands"
  (with-test-db
    (subtest "Testing hset hget hgetall hexists hincr hdel hclear 1"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hget "key1" "field1")          "1000")
      (is (ssdb:hexists "key1" "field1")       t)
      (is (ssdb:hgetall "key1") '("field1" "1000") :test #'equal)
      (is (ssdb:hincr "key1" "field1")         1001)
      (is (ssdb:hincr "key1" "field1" 3)       1004)
      (is (ssdb:hclear "key1")                 1))
    (subtest "Testing hset hget hgetall hexists hdel hclear 2"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hset "key1" "field1" 1000)     nil)
      (is (ssdb:hclear "key1")                 1))
    (subtest "Testing hset hget hexists hdel hclear 3"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hset "key1" "field2" 2000)     t)
      (is (ssdb:hgetall "key1")
          '("field1" "1000" "field2" "2000")
          :test #'equal)
      (is (ssdb:hclear "key1")                 2))
    (subtest "Testing hset hget hexists hsize hdel hclear 4"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hset "key1" "field2" 2000)     t)
      (is (ssdb:hsize "key1")                  2)
      (is (ssdb:hdel "key1" "field2")          t)
      (is (ssdb:hexists "key1" "field2")       nil)
      (is (ssdb:hsize "key1")                  1)
      (is (ssdb:hget "key1" "field2")          nil)
      (is (ssdb:hclear "key1")                 1))
    (subtest "Testing hlist hrlist"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hset "key2" "field1" 1000)     t)
      (is (ssdb:hset "key3" "field1" 1000)     t)
      (is (ssdb:hset "key4" "field1" 1000)     t)
      (is (ssdb:hlist "" "" -1)
          '("key1" "key2" "key3" "key4")
          :test #'equal)
      (is (ssdb:hlist "" "" 2)
          '("key1" "key2")
          :test #'equal)
      (is (ssdb:hlist "key1" "key3" -1)
          '("key2" "key3")
          :test #'equal)
      (is (ssdb:hrlist "" "" -1)
          '("key4" "key3" "key2" "key1")
          :test #'equal)
      (is (ssdb:hrlist "key4" "key2" -1)
          '("key3" "key2")
          :test #'equal)
      (is (ssdb:hclear "key1")                 1)
      (is (ssdb:hclear "key2")                 1)
      (is (ssdb:hclear "key3")                 1)
      (is (ssdb:hclear "key4")                 1))
    (subtest "Testing hkeys hscan hrscan"
      (is (ssdb:hset "key1" "field1" 1000)     t)
      (is (ssdb:hset "key1" "field2" 2000)     t)
      (is (ssdb:hset "key1" "field3" 3000)     t)
      (is (ssdb:hset "key1" "field4" 4000)     t)
      (is (ssdb:hkeys "key1" "" "" -1)
          '("field1" "field2" "field3" "field4")
          :test #'equal)
      (is (ssdb:hkeys "key1" "field1" "field3" -1)
          '("field2" "field3")
          :test #'equal)
      (is (ssdb:hscan "key1" "" "" -1)
          '("field1" "1000" "field2" "2000" "field3" "3000" "field4" "4000")
          :test #'equal)
      (is (ssdb:hscan "key1" "field1" "field3" -1)
          '("field2" "2000" "field3" "3000")
          :test #'equal)
      (is (ssdb:hrscan "key1" "" "" -1)
          '("field4" "4000" "field3" "3000" "field2" "2000" "field1" "1000")
          :test #'equal)
      (is (ssdb:hrscan "key1" "field4" "field2" -1)
          '("field3" "3000" "field2" "2000")
          :test #'equal)

      (is (ssdb:hclear "key1")                 4))
    (subtest "Testing multi_hset multi_hget multi_hdel"
      (is (ssdb:multi_hset "key1" "field1" 1000 "field2" 2000 "field3" 3000) "ok")
      (is (ssdb:multi_hget "key1" "field1" "field2" "field3")
          '("field1" "1000" "field2" "2000" "field3" "3000")
          :test #'equal)
      (is (ssdb:multi_hdel "key1" "field1" "field2") 2)
      (is (ssdb:multi_hget "key1" "field1" "field2" "field3")
          '("field3" "3000")
          :test #'equal)
      (is (ssdb:hclear "key1")                 1))))

(subtest "Testing sorted set commands"
  (with-test-db
    (subtest "Testing zset zget zincr zexists zsize zdel zclear"
      (is (ssdb:zget "not-exist-key" "field1") nil)
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zget "key1" "field1")          1)
      (is (ssdb:zget "key1" "not-exist-field1") nil)
      (is (ssdb:zset "key1" "field1" 2)        nil)
      (is (ssdb:zget "key1" "field1")          2)
      (is (ssdb:zincr "key1" "field1" 1)       3)
      (is (ssdb:zget "key1" "field1")          3)
      (is (ssdb:zexists "key1" "field1")       t)
      (is (ssdb:zsize "key1")                  1)
      (is (ssdb:zset "key1" "field2" 20)       t)
      (is (ssdb:zsize "key1")                  2)
      (is (ssdb:zdel "key1" "field1")          "ok")
      (is (ssdb:zexists "key1" "field1")       nil)
      (is (ssdb:zdel "key1" "field2")          "ok")
      (is (ssdb:zexists "key1" "field2")       nil)
      (is (ssdb:zclear "key1")                 0)
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 20)       t)
      (is (ssdb:zclear "key1")                 2)))
  (with-test-db
    (subtest "Testing zlist zrlist"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key2" "field1" 2)        t)
      (is (ssdb:zset "key3" "field1" 3)        t)
      (is (ssdb:zset "key4" "field1" 4)        t)
      (is (ssdb:zlist "" "" -1) '("key1" "key2" "key3" "key4") :test #'equal)
      (is (ssdb:zlist "" "" 2) '("key1" "key2") :test #'equal)
      (is (ssdb:zlist "key1" "" -1) '("key2" "key3" "key4") :test #'equal)
      (is (ssdb:zlist "key1" "key3" -1) '("key2" "key3") :test #'equal)
      (is (ssdb:zrlist "" "" -1) '("key4" "key3" "key2" "key1") :test #'equal)
      (is (ssdb:zrlist "" "" 2) '("key4" "key3") :test #'equal)
      (is (ssdb:zrlist "key4" "" -1) '("key3" "key2" "key1") :test #'equal)
      (is (ssdb:zrlist "key3" "key1" -1) '("key2" "key1") :test #'equal)))
  (with-test-db
    (subtest "Testing zkeys zscan zrscan"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 2)        t)
      (is (ssdb:zset "key1" "field3" 3)        t)
      (is (ssdb:zset "key1" "field4" 4)        t)
      (is (ssdb:zkeys "key1" "" "" "" -1)
          '("field1" "field2" "field3" "field4")
          :test #'equal)
      (is (ssdb:zkeys "key1" "" "" "" 2) '("field1" "field2") :test #'equal)
      (is (ssdb:zkeys "key1" "" 1 4 -1)
          '("field1" "field2" "field3" "field4")
          :test #'equal)
      (is (ssdb:zkeys "key1" "" 2 3 -1) '("field2" "field3") :test #'equal)
      (is (ssdb:zkeys "key1" "" 2 2 1) '("field2") :test #'equal)
      (is (ssdb:zkeys "key1" "field2" "" "" -1) '("field3" "field4") :test #'equal)
      (is (ssdb:zkeys "key1" "field2" 4 "" -1) '("field4") :test #'equal)))
  (with-test-db
    (subtest "Testing zrank zrrank zsum zavg"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 3)        t)
      (is (ssdb:zset "key1" "field3" 2)        t)
      (is (ssdb:zset "key1" "field4" 5)        t)
      (is (ssdb:zrank "key1" "field1")         0)
      (is (ssdb:zrank "key1" "field2")         2)
      (is (ssdb:zrank "key1" "field3")         1)
      (is (ssdb:zrank "key1" "field4")         3)
      (is (ssdb:zrrank "key1" "field1")        3)
      (is (ssdb:zrrank "key1" "field2")        1)
      (is (ssdb:zrrank "key1" "field3")        2)
      (is (ssdb:zrrank "key1" "field4")        0)
      (is (ssdb:zsum "key1" "" "")             11)
      (is (ssdb:zsum "key1" 0 10)              11)
      (is (ssdb:zsum "key1" 0 3)               6)
      (is (ssdb:zsum "key1" 1 3)               6)
      (is (ssdb:zavg "key1" 0 3)               2.0)
      (is (ssdb:zavg "key1" 1 3)               2.0)
      (is (ssdb:zavg "key1" 2 3)               2.5)
      (is (ssdb:zavg "key1" 2 6)               3.333333)))
  (with-test-db
    (subtest "Testing zremrangebyrank"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 3)        t)
      (is (ssdb:zset "key1" "field3" 2)        t)
      (is (ssdb:zset "key1" "field4" 5)        t)
      (is (ssdb:zremrangebyrank "key1" 0 2)    3)
      (is (ssdb:zget "key1" "field1")          nil)
      (is (ssdb:zget "key1" "field2")          nil)
      (is (ssdb:zget "key1" "field3")          nil)
      (is (ssdb:zget "key1" "field4")          5)))
  (with-test-db
    (subtest "Testing zremrangebyscore"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 3)        t)
      (is (ssdb:zset "key1" "field3" 2)        t)
      (is (ssdb:zset "key1" "field4" 5)        t)
      (is (ssdb:zremrangebyscore "key1" 3 6)   2)
      (is (ssdb:zget "key1" "field1")          1)
      (is (ssdb:zget "key1" "field2")          nil)
      (is (ssdb:zget "key1" "field3")          2)
      (is (ssdb:zget "key1" "field4")          nil)))
  (with-test-db
    (subtest "Testing zpop_front"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 3)        t)
      (is (ssdb:zset "key1" "field3" 2)        t)
      (is (ssdb:zset "key1" "field4" 5)        t)
      (is (ssdb:zpop_front "key1" 2)
          '("field1" "1" "field3" "2")
          :test #'equal)
      (is (ssdb:zpop_front "key1" 1) '("field2" "3") :test #'equal)
      (is (ssdb:zcount "key1" "" "")           1)))
  (with-test-db
    (subtest "Testing zpop_back"
      (is (ssdb:zset "key1" "field1" 1)        t)
      (is (ssdb:zset "key1" "field2" 3)        t)
      (is (ssdb:zset "key1" "field3" 2)        t)
      (is (ssdb:zset "key1" "field4" 5)        t)
      (is (ssdb:zpop_back "key1" 3)
          '("field4" "5" "field2" "3" "field3" "2")
          :test #'equal)))
  (with-test-db
    (subtest "Testing multi_zset multi_zget multi_zdel"
      (is (ssdb:multi_zset "key1" "field1" 1 "field2" 3 "field3" 2 "field4" 5) 4)
      (is (ssdb:multi_zget "key1" "field1" "field2")
          '("field1" "1" "field2" "3")
          :test #'equal)
      (is (ssdb:multi_zget "key1" "field1" "not-exists") '("field1" "1") :test #'equal)
      (is (ssdb:multi_zdel "key1" "field1" "field2") 2)
      (is (ssdb:multi_zdel "key1" "field3" "not-exists") 1))))

(subtest "Testing list commands"
  (with-test-db
    (subtest "Testing qpush_front qpush_back qpush"
      (is (ssdb:qpush_front "key1" "item1" "item2") 2)
      (is (ssdb:qsize "key1")                       2)
      (is (ssdb:qpush_back "key1" "item4" "item5")  4)
      (is (ssdb:qsize "key1")                       4)
      (is (ssdb:qpush "key1" "item6" "item7")       6)
      (is (ssdb:qsize "key1")                       6)
      (is (ssdb:qget "key1" 0)                      "item2")
      (is (ssdb:qget "key1" 1)                      "item1")
      (is (ssdb:qget "key1" 2)                      "item4")
      (is (ssdb:qget "key1" 3)                      "item5")
      (is (ssdb:qget "key1" 4)                      "item6")
      (is (ssdb:qget "key1" 5)                      "item7")
      (is (ssdb:qget "key1" 6)                      nil)
      (is (ssdb:qget "key1" 7)                      nil)
      (is (ssdb:qfront "key1")                      "item2")
      (is (ssdb:qback "key1")                       "item7")
      (is (ssdb:qpop_front "key1")                  '("item2") :test #'equal)
      (is (ssdb:qpop_front "key1" 2)                '("item1" "item4") :test #'equal)
      (is (ssdb:qfront "key1")                      "item5")
      (is (ssdb:qpop_back "key1")                   '("item7") :test #'equal)
      (is (ssdb:qpop_back "key1" 1)                 '("item6") :test #'equal)
      (is (ssdb:qback "key1")                       "item5")
      (is (ssdb:qset "key1" 0 "new-item1")          "ok")
      (is-error (ssdb:qset "key1" 1 "new-item2")    'ssdb:ssdb-error-reply) ;; Index out of range
      (is (ssdb:qget "key1" 0)                      "new-item1")
      (is (ssdb:qget "key1" 1)                      nil)
      (is (ssdb:qclear "key1")                      1)
      (is (ssdb:qsize "key1")                       0)))
  (with-test-db
    (subtest "Testing qrange qslice qtrim_front qtrim_back"
      (is (ssdb:qpush "key1" "item1" "item2" "item3" "item4") 4)
      (is (ssdb:qrange "key1" 0 3) '("item1" "item2" "item3") :test #'equal)
      (is (ssdb:qrange "key1" 1 3) '("item2" "item3" "item4") :test #'equal)
      (is (ssdb:qslice "key1" 1 3) '("item2" "item3" "item4") :test #'equal)
      (is (ssdb:qrange "key1" 1 3) '("item2" "item3" "item4") :test #'equal)
      (is (ssdb:qtrim_front "key1" 2) 2)
      (is (ssdb:qfront "key1")        "item3")
      (is (ssdb:qtrim_back "key1" 1)  1)
      (is (ssdb:qfront "key1")        "item3")))
  (with-test-db
    (subtest "Testing qlist qrlist"
      (is (ssdb:qpush "key1" "item1")  1)
      (is (ssdb:qpush "key2" "item1")  1)
      (is (ssdb:qpush "key3" "item1")  1)
      (is (ssdb:qpush "key4" "item1")  1)
      (is (ssdb:qpush "akey1" "item1") 1)
      (is (ssdb:qpush "bkey2" "item1") 1)
      (is (ssdb:qpush "ckey3" "item1") 1)
      (is (ssdb:qpush "dkey4" "item1") 1)
      (is (ssdb:qlist "key" "" -1)
          '("key1" "key2" "key3" "key4")
          :test #'equal)
      (is (ssdb:qlist "key2" "" -1)
          '("key3" "key4")
          :test #'equal)
      (is (ssdb:qlist "akey" "key" -1)
          '("akey1" "bkey2" "ckey3" "dkey4")
          :test #'equal))))

(subtest "Testing non-ASCII character"
  (with-test-db
    (is (ssdb:set "key" "value contains non-ascii character字符")
        "ok")
    (is (ssdb:get "key")
        "value contains non-ascii character字符")
    (is (ssdb:set "key contains non-ascii character字符"
                  "value contains non-ascii character字符")
        "ok")
    (is (ssdb:get "key contains non-ascii character字符")
        "value contains non-ascii character字符")))
        
(finalize)
