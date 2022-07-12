points_in_a_circle <- function(c_d1,c_d2,n=100,R=1){
t = 2*pi*runif(n)
r = R*sqrt(runif(n))
d1 = c_d1 + r*cos(t)
d2 = c_d2 + r*sin(t)
return(tibble(d1,d2))
}
