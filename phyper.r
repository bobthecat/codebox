# of white balls drawn w/o replacement
q=6-1
# of white ball in the urn
m=28
# of black balls
n=15006-28
# of ball drawn from the urn
k=2426
phyper(q, m, n, k, lower.tail = FALSE)
# [1] 0.2935694
rm(q,m,n,k)

