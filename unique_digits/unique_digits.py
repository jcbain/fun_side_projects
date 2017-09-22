from collections import Counter

def separate_digits(val):
    return [int(x) for x in str(val)]

def any_intersection(x,y):
    inter = set(x).intersection(set(y))
    return bool(inter)

def run_exponential(exp,upper_bound = 10000):

    candidates_result = []
    candidates_base = []
    for i in range(0,upper_bound):
        num = i ** exp

        compound = str(i) + str(exp)
        separate_compound = separate_digits(compound)
        separate_num = separate_digits(num)

        base_digit_counts = list(Counter(separate_digits(i)).values())

        intersection_check = not any_intersection(separate_num,separate_compound)
        base_check = not any(i > 1 for i in base_digit_counts)

        if intersection_check & base_check:
           candidates_result.append(num)
           candidates_base.append(i)

    return candidates_base,candidates_result

if __name__ == '__main__':
    print(run_exponential(6, 1000000000))
