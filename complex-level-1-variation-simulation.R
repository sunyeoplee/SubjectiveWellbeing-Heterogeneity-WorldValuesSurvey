# packages
library(ggplot2)
library(gridExtra)

### complex variation by a continuous variable
{
  #Set sample size 
  n<-500
  set.seed(5)
  
  #plot1
  {
    #Generate a single covariate. 
    x<-runif(n, min = 0, max = 100)
    #Generate an error term, which is normally distributed with mean 0 and SD 4 
    sd<-runif(n, min=0, max=4)
    sd<-sd*10
    residual<-rnorm(n,0,sd*x) 
    #Choose parameters (intercept and slope) and generate the outcome: 
    y <- residual
    continuous <- data.frame(y,x,sd,residual)
    # plot
    plot1 <- ggplot(data=continuous,aes(y=y, x=x)) +
      geom_point(size=rel(0.5)) + 
      labs(x="(b)") +
      theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
            axis.text=element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(),
            axis.ticks = element_blank(),
            axis.line = element_line(),
            panel.background = element_blank())
    plot1
  }
  
  #plot2
  {
    #Generate a single covariate. 
    x<-runif(n, min = 0, max = 100)
    #Generate an error term, which is normally distributed with mean 0 and SD 4 
    sd<-runif(n, min=100, max=105)
    residual<-rnorm(n,0,sd-x) 
    #Choose parameters (intercept and slope) and generate the outcome: 
    y <- (-2)*x + residual
    continuous <- data.frame(y,x,sd,residual)
    # plot
    library(ggplot2)
    plot2 <- ggplot(data=continuous,aes(y=y, x=x)) +
      geom_point(size=rel(0.5)) + 
      labs(x="(a)") +
      theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
            axis.title.y = element_blank(),
            axis.title.x = element_text(),
            axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
    plot2
  }
  
  #plot3
  {
    
    plot3
    
  }  

grid.arrange(plot2, plot1, plot3, ncol=3, bottom = "X", left = "Y")    
  
}




### nonmonotonic heteroscedasticity

# Generate a single covariate. 
x<-runif(n, min = -50, max = 50)
# Generate an error term, which is normally distributed with mean 0 and SD 4 
sd <- -x^2+ 3000 # plot(x, sigma2)
residual<-rnorm(n,0,sd)
#Choose parameters (intercept and slope) and generate the outcome: 
y <- residual
continuous <- data.frame(y,x,sd,residual)
# plot
library(ggplot2)
plot3 <- ggplot(data=continuous,aes(y=y, x=x)) +
  geom_point(size=rel(0.5)) + 
  labs(x="(c)") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())

