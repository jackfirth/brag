#lang racket/base
(require yaragg/examples/baby-json
         (prefix-in alt: yaragg/examples/baby-json-alt)
         yaragg/support
         rackunit)

(let ([str (list "{" 
                 (token 'ID "message")
                 ":"
                 (token 'STRING "'hello world'")
                 "}")]
      [result '(json (object "{"
                             (kvpair "message" ":" (json (string "'hello world'")))
                             "}"))])
  (check-equal? (parse-to-datum str) result)
  (check-equal? (alt:parse-to-datum str) result))


(let ([str "[[[{}]],[],[[{}]]]"]
      [result  '(json
                 (array
                  "["
                  (json (array "[" (json (array "[" (json (object "{" "}")) "]")) "]"))
                  ","
                  (json (array "[" "]"))
                  ","
                  (json (array "[" (json (array "[" (json (object "{" "}")) "]")) "]"))
                  "]"))])
  (check-equal? (parse-to-datum str) result)
  (check-equal? (alt:parse-to-datum str) result))