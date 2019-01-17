# real parameters

p12=0.05
q1=0.02
q2=0.04
rr = q2/q1

prev1=0.6
prev2=0.4
q = prev1 * q1 + prev2 * q2

prev1_2 = prev1 * (1-p12-q1) /(1-q)

prev2_2 = (prev2 * (1-q2) + prev1 * p12) / (1-q)

prev1_2+prev2_2

# suppose

# p12
p12.hat = 1 - prev1_2/prev1 * (1-q) - q/(prev1+rr*prev2)

# q2
q2.hat = 1 - prev2_2/prev2 * (1-q) + prev1/prev2 * p12.hat
