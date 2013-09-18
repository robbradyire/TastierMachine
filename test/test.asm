 ;; sum up the numbers from one to ten
 Const 1
 StoG 0
 ;; address 0 contains the increment
 Const 0
 StoG 1
 ;; address 1 contains the sum
 ;; LOOPSTART
 LoadG 0
 Dup
 LoadG 1
 Add
 StoG 1
 ;; top of stack is the number we added
 Const 1
 Add
 Dup
 StoG 0
 ;; store the new increment, top of stack is the increment
 Const 11
 Equ
 FJmp 4
 ;; if the increment has not reached 10, jump back to LOOPSTART
 ;; finally, write out the total
 LoadG 1
 Write
 Halt
