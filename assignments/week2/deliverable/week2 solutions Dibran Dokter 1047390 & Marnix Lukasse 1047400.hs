-- Assignments Week 2 Dibran Dokter s1047390 & Marnix Lukasse s1047400
--2.2
--See Char.hs
--2.3
{-
If we compute large factorials using :: Int we see that for prod [1..n] with n >= 65 gives a result of 0. We think this is the case because Int has only 64 bits available. When computing large factorials, the numbers always end 
--with a set of 0 digits. For example, 65! (Using Integer) is something like 544....416000000000000. This tail of 0's does never shrink, it only grows. The binary representation of integer will therefore also end with a tail of 0's.
--Int takes 64 bits from the right of the total number, these will then also be only 0-bits. Thats why it shows a result of 0. The reason we observe :: Int showing a negative number sometimes, is because the bits overflow and the 
most significant bit (which indicates positive/negative) gets touched with (too) large numbers.
-}
