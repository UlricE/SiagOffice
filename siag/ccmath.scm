
; Helper for cc_solv, because there is no way to define a Scheme
; function from C with this many parameters.
(define (cc_solv ad ax1 ay1 ax2 ay2 bd bx1 by1 bx2 by2)
  (cc_solv1 (cons ax1 ay1) (cons ax2 ay2) (cons bx1 by1) (cons bx2 by2)))

