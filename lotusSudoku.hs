--Lotus Sudoku

import Debug.Trace
--a sample input
test_arr :: [Int]
test_arr = [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]

--other sample inputs
t1::[Int]
t1 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]

t2::[Int]
t2 = [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0] 

t3::[Int]
t3 = [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]

t4::[Int]
t4= [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]

t5::[Int]
t5= [0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0]

t6::[Int]
t6= [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]


--invalid inputs
t_bad :: [Int]
t_bad = [0,0,3,7,6,1,4,4,0,5,1,7,0,0,0,2,6,0,4,5,3,1,0,0,2,0,0,5,6,0,7,0,0,2,3,2,3,0,7,6,0,0,0,4,5,6,1,7,0]

t_bad2 :: [Int]
t_bad2 = [1,1,1,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]

--answer to the sample input
complete_arr :: [Int]
complete_arr = [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]


--lotusSolver
--input: lotus puzzle
--output: solved lotus puzzle

--If the input grid is solvable, then lotusSolver will solve the puzzle. 
--If not, then it will return an empty list
lotusSolver :: [Int] -> [Int]
lotusSolver inputGrid = if solvable inputGrid inputGrid then (solve [1..7] (zero_ref inputGrid index_ref []) (zero_ref inputGrid index_ref []) inputGrid 0) else []

--solvable: check if there's duplicate numbers in the roses and both the arcs
--input: (sample input to keep track on) (sample_input copy)
--output: True or False
solvable :: [Int] -> [Int] ->  Bool
solvable (x:[]) y = if x == 0 then True else ( if s_123 x arr_check pos then True else False)
		where pos = 48
		      arr_check = take 47 y ++ [0]	
solvable (x:xs) y =  if x == 0 then solvable xs y else ( if s_123 x arr_check pos then solvable xs y else False)
		where pos = (length y - length (x:xs))
		      arr_check = take pos y ++ [0] ++ drop (pos+1) y	

--solve 
--input: (1~7) (index_of_zeros_keepup) (index_of_zeros_ref) (current result) (dropped) = (result)
--output: a solved lotus puzzle
solve :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> [Int]

solve (7:[]) (y:ys:yy) z_ref cur_result dropped
	| ((s_123 7 cur_result y) == False) && (dropped == 0) = [] -- if the puzzle is unsolvable
	| (s_123 7 cur_result y) = solve [1..7] (ys:yy) z_ref ucr (dropped + 1)
	| otherwise = solve [(chkbk + 1) ..7] backzeroindex z_ref backcurrentindex findbeforenot7
			where ucr = update_curr_result 7 y cur_result
			      chkbk = chk_bk dropped z_ref cur_result
			      backzeroindex = bzi dropped z_ref cur_result	
			      backcurrentindex = bcr dropped z_ref cur_result	
			      findbeforenot7 = fbn7 dropped z_ref cur_result	

solve (x:xs) (y:ys:yy) z_ref cur_result dropped	
	| (cur_result !! y == 0) && (s_123 x cur_result y) = solve [1..7] (ys:yy) z_ref ucr (dropped + 1)	
	| otherwise = solve xs (y:ys:yy) z_ref cur_result dropped
			where ucr = update_curr_result x y cur_result

solve (x:xs:xx) (y:[]) z_ref cur_result dropped
	| (s_123 x cur_result y)   = (take y cur_result) ++ [x] ++ (drop (y + 1) cur_result)
	| otherwise = solve (xs:xx) (y:[]) z_ref cur_result dropped

solve (7:[]) (y:[]) z_ref cur_result dropped
	| (s_123 7 cur_result y) = (take y cur_result) ++ [7] ++ (drop (y + 1) cur_result)
	| otherwise = solve [(chkbk + 1) ..7] backzeroindex z_ref backcurrentindex findbeforenot7
			where chkbk = chk_bk dropped z_ref cur_result
			      backzeroindex = bzi dropped z_ref cur_result	
			      backcurrentindex = bcr dropped z_ref cur_result	
			      findbeforenot7 = fbn7 dropped z_ref cur_result		

--update iteratively
--input: value position current_result
update_curr_result :: Int -> Int -> [Int] -> [Int]
update_curr_result value position current_result = (take position current_result) ++ [value] ++ (drop (position + 1) current_result)

--chk_bk: check back the filled number at that position to restart at
--input: (dropped number) (zero_fixed_reference) (current result)
--output: the number filled at the previous solved position
chk_bk :: Int -> [Int] -> [Int] -> Int
chk_bk dropped z_ref cr = cr !! (head (bzi dropped z_ref cr))

--find_before_not_7 
--input: (zero_full_ref) (current_result) (dropped) = 
--output: (index @ edible position for zk_ref   e.g. 0 1 2 3...)     
--edible position has value < 7
fbn7 :: Int -> [Int] -> [Int] -> Int
fbn7 dropped z_ref cr 
	| (dropped - 1) < 0 = -1
	| cr !! (z_ref !! (dropped-1)) < 7 = (dropped - 1)
	| otherwise = fbn7 (dropped -1) z_ref cr 

--back_zeros_index 
--input: (dropped) (zero_full_ref) (current_result) = (updated zero indices (the keep up one, zk_ref))
--update the (keep up) zero indices 
bzi :: Int -> [Int] -> [Int] -> [Int]
bzi dropped z_ref cr = drop (fbn7 dropped z_ref cr) z_ref

--back_current_result 
--input: (dropped number) (z_ref) (current_result)
--output: a list that is ok to increment the number to keep the backtracking going
bcr :: Int -> [Int] -> [Int] -> [Int]
bcr dropped z_ref cr
	| cr !! (z_ref !! (dropped - 1)) < 7 = bcr_1 dropped z_ref cr
	| otherwise = bcr (dropped - 1) z_ref cru
			where cru = bcr_1 dropped z_ref cr

--back_zero_index_by_1
--input: (dropped) (zero_fixed_ref)
--output: (updated zero indices (the keep up one))
--return the (keep up) zero indices by just going back one step
bzi_1 :: Int -> [Int] -> [Int]
bzi_1 dropped z_ref = drop (dropped -1) z_ref

--back_current_result_by_1  
--input: (dropped) (z_fixed_ref) (current_result)
--output: (keep up)result grid by just going back one step
bcr_1 :: Int -> [Int] -> [Int] -> [Int]
bcr_1 dropped z_ref cr = take (head (bzi_1 dropped z_ref)) cr ++ [0] ++ (drop (head (bzi_1 dropped z_ref) + 1) cr)

--zero_ref: zero indices in the original list
--input: (original list) 
--output: a list that has numbers as the index of original list that has value 0
--Use it like this: zero_ref (test_arr) (index_ref) ([])
zero_ref :: [Int] -> [Int] -> [Int] -> [Int]
zero_ref (x:[]) (y:[]) z
	| (x == 0) = (z ++ [y]) 
	| otherwise = z
zero_ref (x:xs) (y:ys) z 
	| (x == 0) = zero_ref xs ys (z ++ [y]) 
	| otherwise = zero_ref xs ys z

--index_ref (use for zero_ref)
index_ref :: [Int]
index_ref = [0..48]

--test all senerios
--input: valueAtCR CurrentResult Position
--output: True or False
s_123 :: Int -> [Int] -> Int -> Bool
s_123 val cr pos = (s1 val cr pos) && (s2 val cr pos) && (s3 val cr pos)

--senario1: check if value at that position is valid in senario 1 
--input: valueAtCR CurrentResult Position
--output: True or False
s1 :: Int -> [Int] -> Int -> Bool
s1 val cr pos
	| elem pos [0..6] = chk_ok val cr [0..6]
	| elem pos [7..13] = chk_ok val cr [7..13]
	| elem pos [14..20] = chk_ok val cr [14..20]
	| elem pos [21..27] = chk_ok val cr [21..27]
	| elem pos [28..34] = chk_ok val cr [28..34]
	| elem pos [35..41] = chk_ok val cr [35..41]
	| elem pos [42..48] = chk_ok val cr [42..48]

--senerio 2 (left leaning arc)
--input: valueAtCR CurrentResult Position
--output: True or False
s2 :: Int -> [Int] -> Int -> Bool
s2 val cr pos
	| elem pos [42,41,34,26,19,11,4] = chk_ok val cr [4,11,19,26,34,41,42]
	| elem pos [48,40,33,25,18,10,3] = chk_ok val cr [3,10,18,25,33,40,48]
	| elem pos [47,39,32,24,17,9,2] = chk_ok val cr [2,9,17,24,32,39,47]
	| elem pos [46,38,31,23,16,8,1] = chk_ok val cr [1,8,16,23,31,38,46]
	| elem pos [45,37,30,22,15,7,0] = chk_ok val cr [0,7,15,22,30,37,45]
	| elem pos [44,36,29,21,14,13,6] = chk_ok val cr [6,13,14,21,29,36,44]
	| elem pos [43,35,28,27,20,12,5] = chk_ok val cr [5,12,20,27,28,35,43]

--senerio 3 (right leaning arc)	
--input: valueAtCR CurrentResult Position
--output: True or False 
s3 :: Int -> [Int] -> Int -> Bool
s3 val cr pos
	| elem pos [42,35,29,22,16,9,3] = chk_ok val cr [42,35,29,22,16,9,3]
	| elem pos [43,36,30,23,17,10,4] = chk_ok val cr [43,36,30,23,17,10,4]
	| elem pos [44,37,31,24,18,11,5] = chk_ok val cr [44,37,31,24,18,11,5]
	| elem pos [45,38,32,25,19,12,6] = chk_ok val cr [45,38,32,25,19,12,6]
	| elem pos [46,39,33,26,20,13,0] = chk_ok val cr [46,39,33,26,20,13,0]
	| elem pos [47,40,34,27,14,7,1] = chk_ok val cr [47,40,34,27,14,7,1]
	| elem pos [48,41,28,21,15,8,2] = chk_ok val cr [48,41,28,21,15,8,2]

--check ok: used in s1 s2 s3
--input: valueAtCR CurrentResult Position
--output: True or False 
chk_ok :: Int -> [Int] -> [Int] -> Bool
chk_ok val current_result [] = True
chk_ok val current_result (x:xs)
	| (current_result !! x) /= val =  chk_ok val current_result xs
	| otherwise = False
	
