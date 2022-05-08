# Given a list of integers, compute the sum of the list

def arraySum : int (nums : list) {
	sum = 0
	for num in nums {
		sum = sum + num
	}
	return sum
}

nums1 = [-2, 3, -4, 5, 9, 8]
sum1 = arraySum(nums1)

nums2 = [1, -1, 1, -1, 1, -1]
sum2 = arraySum(nums2)

# test some comparison operators and logical operators
print(sum1 == 19)
print(sum2 == 0)
print(sum1 >= sum2)
print(not (sum1 > 0 or sum2 < 0))
print(sum1 <= 19 and sum2 != 0)

