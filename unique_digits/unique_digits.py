from collections import Counter
def separate_digits(val):
    return [int(x) for x in str(val)]

def any_intersection(x,y):
    inter = set(x).intersection(set(y))
    return bool(inter)

def run_exponential(exp,upper_bound = 10000):
    
    candidates = []   
    for i in range(0,upper_bound):
        num = i ** exp
        
        compound = str(num) + str(i)
        separate_compound = separate_digits(compound)
        separate_num = separate_digits(num)
      
        if (not any_intersection(separate_compound,separate_num)) & (all(list(Counter(separate_digits(i)).values())) <= 1):
           candidates.append(num)


    return max(candidates)
 
