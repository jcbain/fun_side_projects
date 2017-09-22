from collections import Counter

def separate_digits(val):
    """
    separate the digits of a number in a list of its digits

    Parameters
    -----
    val : int
      number to separate out

    Returns
    -----
    list
      a list of ints of the digits that make up val
    """
    return [int(x) for x in str(val)]

def any_intersection(x,y):
    """
    check to see if there are any intersecting values between two lists

    Parameters
    -----
    x : list
      first list of values

    y : list
      second list of values

    Returns
    -----
    bool
      True if there is an intersection, False otherwise
    """
    inter = set(x).intersection(set(y))
    return bool(inter)

def run_exponential(exp,upper_bound = 10000):
    """
    finds the values between 0 and an upper bound that contains all unique digits
    and shares no digits with its value raised to some exponent

    Parameters
    -----
    exp : int
      exponent to raise the values to

    upper_bound : int
      the upper limit of where the list should stop at

    Returns
    lists
      first list is a list the base values that meet the criteria
      second list is a list of the base values that work raised to the exponent 
    """
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
