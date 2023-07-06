-------------------------------------------------------------------------------
-- |
-- Module      :  Avionics Attitude Estimation System (AAES) w/ ForSyDe.Shallow
-- Copyright   :  (c) Gabriel Castro
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------
module AAES where
import ForSyDe.Shallow

---------------------------
-- Kernels 
---------------------------
aaes_cores = kernel21SADF
voter_core = kernel32SADF

-- voter has 2 outputs: the voted output and 
-- an int value to indicate which core is wrong
voter_function :: (Eq a) => [a] -> [a] -> [a] -> ([a], [Int])
voter_function x y z | x == y && y == z = (x, [0]) -- cores ok
                     | x == y && y /= z = (x, [3]) -- core 3 error
                     | x /= y && y == z = (y, [1]) -- core 1 error
                     | x == z && y /= z = (x, [2]) -- core 2 error
                     | otherwise = (x, [4]) -- 2+ cores with error

-- The voter kernel has 3 inputs, and 2 outputs, each one consumes/produces one token
sf_voter = ((1,1,1),(1,1), voter_function)

---------------------------
-- Detector (cores control)
---------------------------
-- next state functions
next_state 4 [x] = 4
next_state _ [x] = x

-- 'cores_control' functions definition
core_func = ((1,1),1, core_aaes)  
core_new = ((1,1),1, new_func)  

-- scenarios definition
select_scenario 0 = ((1,1,1,1), ([core_func], [core_func], [core_func], [sf_voter]))
select_scenario 1 = ((1,1,1,1), ([core_new],  [core_func], [core_func], [sf_voter1]))
select_scenario 2 = ((1,1,1,1), ([core_func], [core_new],  [core_func], [sf_voter2]))
select_scenario 3 = ((1,1,1,1), ([core_func], [core_func], [core_new],  [sf_voter3]))
select_scenario 4 = ((1,1,1,1), ([core_new],  [core_new],  [core_new],  [sf_voter4]))

-- cores control
cores_control = detector14SADF consume_rate next_state select_scenario initial_state
  where
    consume_rate = 1
    initial_state = 0

---------------------------
-- AAES TMR sys. definition
---------------------------
aaes_tmr_system (w, a) = s_out where
  o_core1 = aaes_cores sf_core1 w a
  o_core2 = aaes_cores sf_core2 w a
  o_core3 = aaes_cores sf_core3 w a
  (s_out,s_cc) = voter_core sf_voter o_core1 o_core2 o_core3
  (sf_core1, sf_core2, sf_core3,sf_voter) = cores_control d_cc
  d_cc = delaySADF [0] s_cc

-- test signals definition
s_w = signal [(8,2,3),(2,3,5),(1,1,1)]
s_a = signal [(2,3,5),(8,2,3),(1,0,1)]

-- to test the system, run the following in ghci
-- ghci> aaes_tmr_system (s_w,s_a)
-- ghci> {(8,2,3,13,10),(2,3,5,10,13),(1,1,1,3,2)}

---------------------------
-- AAES voter functions
-- after a core is reconf
---------------------------
-- voter after core 1 reconfig
voter_function1 :: (Eq a) => [a] -> [a] -> [a] -> ([a], [Int])
voter_function1 x y z | y == z = (y, [1]) -- cores ok
                      | y /= z = (y, [4]) -- core 2 or 3 error

sf_voter1 = ((1,1,1),(1,1), voter_function1)

-- voter after core 2 reconfig
voter_function2 :: (Eq a) => [a] -> [a] -> [a] -> ([a], [Int])
voter_function2 x y z | x == z = (x, [2]) -- cores ok
                      | x /= z = (x, [4]) -- core 1 or 3 error

sf_voter2 = ((1,1,1),(1,1), voter_function2)

-- voter after core 3 reconfig
voter_function3 :: (Eq a) => [a] -> [a] -> [a] -> ([a], [Int])
voter_function3 x y z | x == y = (x, [3]) -- cores ok
                      | x /= y = (x, [4]) -- core 1 or 2 error

sf_voter3 = ((1,1,1),(1,1), voter_function3)

-- voter after all cores reconfig
voter_function4 :: (Eq a) => [a] -> [a] -> [a] -> ([a], [Int])
voter_function4 x y z = (x, [4]) -- 

sf_voter4 = ((1,1,1),(1,1), voter_function4)

---------------------------------------------------------------------------------
-- AAES cores functions
---------------------------------------------------------------------------------
-- Here is where we define the functions that implement the AAES behavior 
-- presented in the paper, since the point is to demonstrate the 
-- implementation of the TMR with reconfiguration using SADF, we defined 
-- two simple functions core_aaes (the primary function) and new_func 
-- (the function that will replace the core_aaes when a failure occurs)
---------------------------------------------------------------------------------
core_aaes :: (Num a) => [(a, a, a)] -> [(a, a, a)] -> [(a, a, a, a, a)]
core_aaes [(x1,x2,x3)] [(y1,y2,y3)] = [(x1, x2, x3, x1 + x2 + x3, y1 + y2 + y3)] 

new_func :: (Num a) => [(a, a, a)] -> [(a, a, a)] -> [(a, a, a, a, a)]
new_func [(x1,x2,x3)] [(y1,y2,y3)] = [(y1, y2, y3, x1 - x2 + x3, y1 - y2 + y3)] 