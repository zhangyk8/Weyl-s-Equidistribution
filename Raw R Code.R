## Visualization of Weyl's equidistribution theorem
N = 10
n = 1:N
irra_seq = sapply(n, function(x){
  x * sqrt(3) - floor(x * sqrt(3))
})
data_seq = data.frame(x = irra_seq, y = rep(0, N))

library(ggplot2)
ggplot(data_seq) + geom_point(mapping = aes(x = x, y = y), color = "red")

## The geometric interpretation of the Weyl's equidistribution theorem
# The slope of the arc
gam = sqrt(2)
N = 100
direct = "right"
point = data.frame(x = c(0.2, rep(0, N)), y = c(0.8, rep(0, N)))

for(i in 1:N){
  b = point$y[i] - gam * point$x[i]
  
  if((direct == "right" & gam + b > 1) | (direct == "left" & b > 1)){
    point$y[i+1] = 1
    point$x[i+1] = (1 - b)/gam
    gam = - gam
  }else if((direct == "right" & gam + b >= 0 & gam + b <= 1)){
    point$x[i+1] = 1
    point$y[i+1] = gam + b
    gam = - gam
    direct = "left"
  }else if((direct == "right" & gam + b < 0) |(direct == "left" & b < 0)){
    point$x[i+1] = -b/gam
    point$y[i+1] = 0
    gam = - gam
  }else if(direct == "left" & b >= 0 & b <= 1){
    point$x[i+1] = 0
    point$y[i+1] = b
    gam = -gam
    direct = "right"
  }
}

point$Type = c("Original Point", rep("Reflecting Points", N))
ggplot(point) + geom_path(mapping = aes(x, y), color = "green") + geom_point(mapping = aes(x, y, color = Type)) + scale_color_manual(values = c("red", "blue"))


