(require "asserts")
(require "ffi")

(defpackage :ffi-size-and-offsets
  (:use lispyboi
        test-suite))

(in-package :ffi-size-and-offsets)

(ffi-defstruct foo
               (x int32)
               (y int32)
               (z int32))

(assert-= 12 (ffi-sizeof foo))
(assert-= 0 (ffi-offset-of foo x))
(assert-= 4 (ffi-offset-of foo y))
(assert-= 8 (ffi-offset-of foo z))

(ffi-defstruct bar
               (a int64)
               (b int64)
               (c int64))

(assert-= 24 (ffi-sizeof bar))
(assert-= 0 (ffi-offset-of bar a))
(assert-= 8 (ffi-offset-of bar b))
(assert-= 16 (ffi-offset-of bar c))

(ffi-defstruct composite
               (n int64)
               (c char)
               (bar bar)
               (foo foo)
               (aligned int))

(assert-= 56 (ffi-sizeof composite))
(assert-= 0 (ffi-offset-of composite n))
(assert-= 8 (ffi-offset-of composite c))
(assert-= 16 (ffi-offset-of composite bar))
(assert-= 40 (ffi-offset-of composite foo))
(assert-= 52 (ffi-offset-of composite aligned))


(ffi-defunion a-union
              (timestamp int64)
              (a-foo foo)
              (foo-ptr foo*))

(assert-= 16 (ffi-sizeof a-union))
(assert-= 0 (ffi-offset-of a-union timestamp))
(assert-= 0 (ffi-offset-of a-union a-foo))
(assert-= 0 (ffi-offset-of a-union foo-ptr))

(ffi-defstruct with-a-union
               (num int*)
               (bar bar)
               (u a-union)
               (foo foo))

(assert-= 64 (ffi-sizeof with-a-union))
(assert-= 0 (ffi-offset-of with-a-union num))
(assert-= 8 (ffi-offset-of with-a-union bar))
(assert-= 32 (ffi-offset-of with-a-union u))
(assert-= 48 (ffi-offset-of with-a-union foo))

(ffi-defstruct some-ptrs
               (h int*)
               (c1 int32)
               (j char*)
               (c2 int16)
               (k foo*)
               (c3 int8))

(assert-= 48 (ffi-sizeof some-ptrs))
(assert-= 0 (ffi-offset-of some-ptrs h))
(assert-= 8 (ffi-offset-of some-ptrs c1))
(assert-= 16 (ffi-offset-of some-ptrs j))
(assert-= 24 (ffi-offset-of some-ptrs c2))
(assert-= 32 (ffi-offset-of some-ptrs k))
(assert-= 40 (ffi-offset-of some-ptrs c3))

(ffi-defstruct foo2
               (x char*)
               (y int8)
               (z int64))

(assert-= 24 (ffi-sizeof foo2))
(assert-= 0 (ffi-offset-of foo2 x))
(assert-= 8 (ffi-offset-of foo2 y))
(assert-= 16 (ffi-offset-of foo2 z))

(ffi-defstruct int8s
               (x int8)
               (y int8)
               (z int8))

(assert-= 3 (ffi-sizeof int8s))
(assert-= 0 (ffi-offset-of int8s x))
(assert-= 1 (ffi-offset-of int8s y))
(assert-= 2 (ffi-offset-of int8s z))

(ffi-defstruct int16s
               (x int16)
               (y int16)
               (z int16))

(assert-= 6 (ffi-sizeof int16s))
(assert-= 0 (ffi-offset-of int16s x))
(assert-= 2 (ffi-offset-of int16s y))
(assert-= 4 (ffi-offset-of int16s z))

(ffi-defstruct mixed-ascend
               (x int8)
               (y int16)
               (z int32))

(assert-= 8 (ffi-sizeof mixed-ascend))
(assert-= 0 (ffi-offset-of mixed-ascend x))
(assert-= 2 (ffi-offset-of mixed-ascend y))
(assert-= 4 (ffi-offset-of mixed-ascend z))

(ffi-defstruct mixed-descend
               (x int32)
               (y int16)
               (z int8))

(assert-= 8 (ffi-sizeof mixed-descend))
(assert-= 0 (ffi-offset-of mixed-descend x))
(assert-= 4 (ffi-offset-of mixed-descend y))
(assert-= 6 (ffi-offset-of mixed-descend z))

(ffi-defunion mixed-union-2
              (x int8)
              (y int16)
              (z int8))

(assert-= 2 (ffi-sizeof mixed-union-2))
(assert-= 0 (ffi-offset-of mixed-union-2 x))
(assert-= 0 (ffi-offset-of mixed-union-2 y))
(assert-= 0 (ffi-offset-of mixed-union-2 z))

(ffi-defunion mixed-union-4
              (x int8)
              (y int16)
              (z int32))

(assert-= 4 (ffi-sizeof mixed-union-4))
(assert-= 0 (ffi-offset-of mixed-union-4 x))
(assert-= 0 (ffi-offset-of mixed-union-4 y))
(assert-= 0 (ffi-offset-of mixed-union-4 z))

(ffi-defunion mixed-union-8
              (x int64)
              (y int16)
              (z int32))

(assert-= 8 (ffi-sizeof mixed-union-8))
(assert-= 0 (ffi-offset-of mixed-union-8 x))
(assert-= 0 (ffi-offset-of mixed-union-8 y))
(assert-= 0 (ffi-offset-of mixed-union-8 z))

(ffi-defstruct -aux-struct-12
               (a int32)
               (b int32)
               (c int32))
(assert-= 12 (ffi-sizeof -aux-struct-12))

(ffi-defunion mixed-union-12
              (x -aux-struct-12)
              (y int16)
              (z int32))

(assert-= 12 (ffi-sizeof mixed-union-12))
(assert-= 0 (ffi-offset-of mixed-union-12 x))
(assert-= 0 (ffi-offset-of mixed-union-12 y))
(assert-= 0 (ffi-offset-of mixed-union-12 z))

(ffi-defunion mixed-union-16
              (x -aux-struct-12)
              (y int64)
              (z int8))

(assert-= 16 (ffi-sizeof mixed-union-16))
(assert-= 0 (ffi-offset-of mixed-union-16 x))
(assert-= 0 (ffi-offset-of mixed-union-16 y))
(assert-= 0 (ffi-offset-of mixed-union-16 z))

(ffi-defstruct qux-inner
               (p char*)
               (x short))
(ffi-defstruct qux
               (c char)
               (inner qux-inner))
(assert-= 24 (ffi-sizeof qux))
(assert-= 0 (ffi-offset-of qux c))
(assert-= 8 (ffi-offset-of qux inner))

;; These were tested and verified with this C program compiled for 64-bit intel arch:
;;
;; #include <stddef.h>
;; #include <stdio.h>
;; #include <stdint.h>
;; 
;; int main(int argc, char **argv)
;; {
;; 
;;     struct foo
;;     {
;;         int32_t x;
;;         int32_t y;
;;         int32_t z;
;;     };
;;     printf("sizeof(foo) = %ld\n", sizeof(struct foo));
;;     printf("offset of foo.x = %ld\n", offsetof(struct foo, x));
;;     printf("offset of foo.y = %ld\n", offsetof(struct foo, y));
;;     printf("offset of foo.z = %ld\n", offsetof(struct foo, z));
;; 
;;     struct bar
;;     {
;;         int64_t a;
;;         int64_t b;
;;         int64_t c;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(bar) = %ld\n", sizeof(struct bar));
;;     printf("offset of bar.a = %ld\n", offsetof(struct bar, a));
;;     printf("offset of bar.b = %ld\n", offsetof(struct bar, b));
;;     printf("offset of bar.c = %ld\n", offsetof(struct bar, c));
;; 
;;     struct composite
;;     {
;;         int64_t n;
;;         char c;
;;         struct bar bar;
;;         struct foo foo;
;;         int32_t aligned;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(composite) = %ld\n", sizeof(struct composite));
;;     printf("offset of composite.n = %ld\n", offsetof(struct composite, n));
;;     printf("offset of composite.c = %ld\n", offsetof(struct composite, c));
;;     printf("offset of composite.bar = %ld\n", offsetof(struct composite, bar));
;;     printf("offset of composite.foo = %ld\n", offsetof(struct composite, foo));
;;     printf("offset of composite.aligned = %ld\n", offsetof(struct composite, aligned));
;; 
;;     union a_union
;;     {
;;         int64_t timestamp;
;;         struct foo a_foo;
;;         //struct foo *foo_ptr;
;;         uint64_t foo_ptr;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(a_union) = %ld\n", sizeof(union a_union));
;;     printf("offset of a_union.timestamp = %ld\n", offsetof(union a_union, timestamp));
;;     printf("offset of a_union.a_foo = %ld\n", offsetof(union a_union, a_foo));
;;     printf("offset of a_union.foo_ptr = %ld\n", offsetof(union a_union, foo_ptr));
;; 
;;     struct with_a_union
;;     {
;;         int *num;
;;         struct bar bar;
;;         union a_union u;
;;         struct foo foo;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(with_a_union) = %ld\n", sizeof(struct with_a_union));
;;     printf("offset of with_a_union.num = %ld\n", offsetof(struct with_a_union, num));
;;     printf("offset of with_a_union.bar = %ld\n", offsetof(struct with_a_union, bar));
;;     printf("offset of with_a_union.u = %ld\n", offsetof(struct with_a_union, u));
;;     printf("offset of with_a_union.foo = %ld\n", offsetof(struct with_a_union, foo));
;; 
;;     struct some_ptrs
;;     {
;;         int *h;
;;         int32_t c1;
;;         char *j;
;;         int16_t c2;
;;         struct foo *k;
;;         int8_t c3;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(some_ptrs) = %ld\n", sizeof(struct some_ptrs));
;;     printf("offset of some_ptrs.h = %ld\n", offsetof(struct some_ptrs, h));
;;     printf("offset of some_ptrs.c1 = %ld\n", offsetof(struct some_ptrs, c1));
;;     printf("offset of some_ptrs.j = %ld\n", offsetof(struct some_ptrs, j));
;;     printf("offset of some_ptrs.c2 = %ld\n", offsetof(struct some_ptrs, c2));
;;     printf("offset of some_ptrs.k = %ld\n", offsetof(struct some_ptrs, k));
;;     printf("offset of some_ptrs.c3 = %ld\n", offsetof(struct some_ptrs, c3));
;; 
;;     struct foo2
;;     {
;;         char *x;
;;         int8_t y;
;;         int64_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(foo2) = %ld\n", sizeof(struct foo2));
;;     printf("offset of foo2.x = %ld\n", offsetof(struct foo2, x));
;;     printf("offset of foo2.y = %ld\n", offsetof(struct foo2, y));
;;     printf("offset of foo2.z = %ld\n", offsetof(struct foo2, z));
;; 
;;     struct int8s
;;     {
;;         int8_t x;
;;         int8_t y;
;;         int8_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(int8s) = %ld\n", sizeof(struct int8s));
;;     printf("offset of int8s.x = %ld\n", offsetof(struct int8s, x));
;;     printf("offset of int8s.y = %ld\n", offsetof(struct int8s, y));
;;     printf("offset of int8s.z = %ld\n", offsetof(struct int8s, z));
;; 
;;     struct int16s
;;     {
;;         int16_t x;
;;         int16_t y;
;;         int16_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(int16s) = %ld\n", sizeof(struct int16s));
;;     printf("offset of int16s.x = %ld\n", offsetof(struct int16s, x));
;;     printf("offset of int16s.y = %ld\n", offsetof(struct int16s, y));
;;     printf("offset of int16s.z = %ld\n", offsetof(struct int16s, z));
;; 
;;     struct mixed_ascend
;;     {
;;         int8_t x;
;;         int16_t y;
;;         int32_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_ascend) = %ld\n", sizeof(struct mixed_ascend));
;;     printf("offset of mixed_ascend.x = %ld\n", offsetof(struct mixed_ascend, x));
;;     printf("offset of mixed_ascend.y = %ld\n", offsetof(struct mixed_ascend, y));
;;     printf("offset of mixed_ascend.z = %ld\n", offsetof(struct mixed_ascend, z));
;; 
;;     struct mixed_descend
;;     {
;;         int32_t x;
;;         int16_t y;
;;         int8_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_descend) = %ld\n", sizeof(struct mixed_descend));
;;     printf("offset of mixed_descend.x = %ld\n", offsetof(struct mixed_descend, x));
;;     printf("offset of mixed_descend.y = %ld\n", offsetof(struct mixed_descend, y));
;;     printf("offset of mixed_descend.z = %ld\n", offsetof(struct mixed_descend, z));
;; 
;;     union mixed_union_2
;;     {
;;         int8_t x;
;;         int16_t y;
;;         int8_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_union_2) = %ld\n", sizeof(union mixed_union_2));
;;     printf("offset of mixed_union_2.x = %ld\n", offsetof(union mixed_union_2, x));
;;     printf("offset of mixed_union_2.y = %ld\n", offsetof(union mixed_union_2, y));
;;     printf("offset of mixed_union_2.z = %ld\n", offsetof(union mixed_union_2, z));
;; 
;;     union mixed_union_4
;;     {
;;         int8_t x;
;;         int16_t y;
;;         int32_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_union_4) = %ld\n", sizeof(union mixed_union_4));
;;     printf("offset of mixed_union_4.x = %ld\n", offsetof(union mixed_union_4, x));
;;     printf("offset of mixed_union_4.y = %ld\n", offsetof(union mixed_union_4, y));
;;     printf("offset of mixed_union_4.z = %ld\n", offsetof(union mixed_union_4, z));
;; 
;;     union mixed_union_8
;;     {
;;         int64_t x;
;;         int16_t y;
;;         int32_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_union_8) = %ld\n", sizeof(union mixed_union_8));
;;     printf("offset of mixed_union_8.x = %ld\n", offsetof(union mixed_union_8, x));
;;     printf("offset of mixed_union_8.y = %ld\n", offsetof(union mixed_union_8, y));
;;     printf("offset of mixed_union_8.z = %ld\n", offsetof(union mixed_union_8, z));
;; 
;;     union mixed_union_12
;;     {
;;         struct {
;;             int32_t a;
;;             int32_t b;
;;             int32_t c;
;;         } x;
;;         int16_t y;
;;         int32_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_union_12) = %ld\n", sizeof(union mixed_union_12));
;;     printf("offset of mixed_union_12.x = %ld\n", offsetof(union mixed_union_12, x));
;;     printf("offset of mixed_union_12.y = %ld\n", offsetof(union mixed_union_12, y));
;;     printf("offset of mixed_union_12.z = %ld\n", offsetof(union mixed_union_12, z));
;; 
;;     union mixed_union_16
;;     {
;;         struct {
;;             int32_t a;
;;             int32_t b;
;;             int32_t c;
;;         } x;
;;         int64_t y;
;;         int8_t z;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(mixed_union_16) = %ld\n", sizeof(union mixed_union_16));
;;     printf("offset of mixed_union_16.x = %ld\n", offsetof(union mixed_union_16, x));
;;     printf("offset of mixed_union_16.y = %ld\n", offsetof(union mixed_union_16, y));
;;     printf("offset of mixed_union_16.z = %ld\n", offsetof(union mixed_union_16, z));
;; 
;;     struct qux {
;;         char c;
;;         struct qux_inner {
;;             char *p;
;;             short x;
;;         } inner;
;;     };
;; 
;;     printf("\n");
;;     printf("sizeof(qux) = %ld\n", sizeof(struct qux));
;;     printf("offset of qux.c = %ld\n", offsetof(struct qux, c));
;;     printf("offset of qux.inner = %ld\n", offsetof(struct qux, inner));
;;     return 0;
;; }
