#include <stddef.h>

/*@
    requires \valid_range(arr, 0, n-1);
    requires n > 0;

    assigns \nothing;

    ensures \forall integer i; 0 <= i < n ==> \result >= arr[i];
*/
int find_max(const int *arr, size_t n) {
    int max = arr[0];
    /*@
        loop invariant 1 <= i <= n;
        loop invariant \forall integer j; 0 <= j < i ==> max >= arr[j];
        loop variant n - i;
    */
    for (size_t i = 1; i < n; ++i) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    return max;
}

int main() {
    int array[] = {5, 2, 8, 3, 9, 4};
    size_t size = sizeof(array) / sizeof(array[0]);

    int sum = add(10, 20);
    int max = find_max(array, size);

    return 0;
}
