# Create a function to project the population

pop_proj<- function(growth, tmax){
  n <- tibble(n=0, time=1:tmax)
  n$n[1] <- 100
  for (t in 2:tmax){  
    # back-transform to get lambda 
    lambda <- exp(growth) 
    #project one time step from the current pop size
    n$n[t]=n$n[[(t-1)]]*lambda 
  }
  return(n)
}

# Run the function with example parameters
proj_pop <- pop_proj(growth=0.5, # Growth rate
  tmax = 100          # Number of time steps for projection
)

# Plot results to visualize population changes over time
library(ggplot2)

(pop_plot <- ggplot(proj_pop, aes(x=time, y=n))+
  geom_point()+ geom_line()+
    theme_bw())
