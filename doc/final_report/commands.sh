frama-c test_files/test1.c -cpp-extra-args="-Itest_files/include_folder -Itest_files/include_folder2" -lsp -lsp-debug=0 -lsp-declaration="/home/user/git/L1/T0304764/acsl
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