frama-c test_files/test1.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -lsp -lsp-debug=0 -lsp-definition="/home/user/git/L1/T0304764/acsl
_lsp/Acsl_lsp/server_v3/test_files/test1.c:88:8"

frama-c test_files/test1.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -lsp -lsp-debug=0 -lsp-declaration="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1.c:89:12"

frama-c test_files/folder1/test2.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -lsp -lsp-debug=0 -lsp-display-cil

frama-c test_files/test1.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -then -cg="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1.dot" -cg-services -then -lsp -lsp-debug=0 -lsp-compute-cg="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1"

frama-c test_files/test1.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -then -metrics -mtrics-by-function -metrics-output="/home/us
er/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1.txt" -then -lsp -lsp-debug=0 -lsp-metrics="/home/us
er/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1"

frama-c test_files/test1.c test_files/folder1/test2.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -then -metrics -mtrics-by-function -metrics-output="/home/us
er/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/project_metrics.txt" -then -lsp -lsp-debug=0 -lsp-metrics="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/project_metrics"

frama-c /home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1.c -cpp-extra-args="-I/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/include_folder -I/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/include_folder2" -then -lsp -lsp-root-path="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files" -lsp-show-povc="/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/test_files/test1.c:29:27" -wp -wp-gen

frama-c -cpp-extra-args="-I/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/test_files/include_folder -I/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/test_files/include_folder2" -kernel-warn-key annot-error=active -no-unicode -lsp -lsp-no-cmdline -lsp-debug=3  -lsp-did-save=/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/test_files/test1.c

#### COMMNDS WITH ESCAPED UNDERSCORES FOR LATEX

frama-c test\_files/test1.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -lsp -lsp-debug=0 -lsp-definition="/home/user/git/L1/T0304764/acsl
\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.c:88:8"

frama-c test\_files/test1.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -lsp -lsp-debug=0 -lsp-declaration="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.c:89:12"

frama-c test\_files/folder1/test2.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -lsp -lsp-debug=0 -lsp-display-cil

frama-c test\_files/test1.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -then -cg="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.dot" -cg-services -then -lsp -lsp-debug=0 -lsp-compute-cg="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1"

frama-c test\_files/test1.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -then -metrics -mtrics-by-function -metrics-output="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.txt" -then -lsp -lsp-debug=0 -lsp-metrics="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1"

frama-c test\_files/test1.c test\_files/folder1/test2.c -cpp-extra-args="-Itest\_files/include\_folder -Itest\_files/include\_folder2" -then -metrics -mtrics-by-function -metrics-output="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/project\_metrics.txt" -then -lsp -lsp-debug=0 -lsp-metrics="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/project\_metrics"

frama-c /home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.c -cpp-extra-args="-I/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/include\_folder -I/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/include\_folder2" -then -lsp -lsp-root-path="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files" -lsp-show-povc="/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server\_v3/test\_files/test1.c:29:27" -wp -wp-gen

frama-c -cpp-extra-args="-I/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server/test\_files/include\_folder -I/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server/test\_files/include\_folder2" -lsp -lsp-debug=4 -lsp-did-save=/home/user/git/L1/T0304764/acsl\_lsp/Acsl\_lsp/server/test\_files/test1.c