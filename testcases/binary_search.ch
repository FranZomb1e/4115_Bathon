li : int[]

def get_list : int[] (a : int[], len : int) {
    init(a)
    i = 0
    while (i < len) {
        append(a, i)
        i = i + 1
    }
    return a
}

def binary_search : int (a : int[], target : int) {
    low = 0
    high = len(a) - 1
    while (low <= high) {
        mid = low + (high - low) / 2
        if (a[mid] == target) {
            return mid
        }
        if (a[mid] < target) {
            low = mid + 1
        } else {
            high = mid - 1
        }
    }
    return -1
}

def print_res : int (idx : int) {
    if (idx == -1) {
        print("target not found")
        return -1
    } else {
        print("target found with idx: ")
        print(idx)
    }
    return 0
}

target = 5
list_1 = get_list(li, 6)
list_2 = get_list(li, 5)
print("Current list: [0,1,2,3,4,5], look for 5")
print_res(binary_search(list_1, target))
print("Current list: [0,1,2,3,4], look for 5")
print_res(binary_search(list_2, target))
