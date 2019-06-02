<img src="https://wet.kuleuven.be/internationalisering/sedes.jpg/image_preview" />


# APLAI 2018-2019

> This is our implementation of the APLAI project.

---

## Task 1: Sudoku
### ECLiPSe
Compile eclipse/sudoku.pl in your TkEclipse.  
Solving a puzzle is done with the solveClassic/1, solveNew/1 and solveBoth/1 predicates.  
The example puzzles and their alternative viewpoints are copied into the file.  
E.g. alt_lambda(P),solveNew(P) solves the lambda puzzle with the alternative viewpoint.  
Benchmarks and backtrack numbers are available with the benchmark{method}/2 and backtrack{method}/2 predicates.  
Here, {method} signifies the solving method: Classic, New or Both.  
### CHR
Compile chr/sudoku_chr.pl in your SWIPL.  
The same predicates as in the ECLiPSe implementation can be used for solving the puzzles.  
The benchmarks were done with the time/1 predicate, so no benchmarks are available in the file.  

## Task 2: Hashiwokakero
### ECLiPSe
Compile eclipse/hashi.pl in your TkEclipse.  
The puzzle benchmarks can be run individually with "?- hashi(PuzzleId)."  
To run all benchmarks use "?- run_benchmark_puzzles."  
### CHR
Consult chr/hashi.pl in your SWIPL.  
The puzzle benchmarks can be run individually with "?- solve(PuzzleId)."  
To get statistics about a benchmark use "?- get_statistics(PuzzleId)."  
### Extra
Also included are:  
 - A python script for visualizing puzzles "draw.py"  
 - The visualisations of the benchmark puzzles "puzzles/"  
 - 2 alternative passive CHR implementations (reachable based/segment based)  
 - Results of the benchmarks  
## Task 3: Planning
Compile benchmarksMeeting.ecl in your TkEclipse.  
Run the tests using the supplied predicates.  
Using the self-implemented disjunctive predicate requires changing the predicate in the benchmarks.  
Again, no benchmarks are available.  

---

