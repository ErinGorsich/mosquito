library("deSolve")
library("ggplot2")
rm(list = ls())
pi.val <- 3.141592653589793

# Trial population growth rate function
#####################################################
pop.growth.rate = function(t, A, omega, theta){
    x = A*cos(omega * t + theta)
    return(x)
}

par(mfrow = c(1, 2))
# Daily time step
times <- seq(0, 5*365, 1) # for daily time step, omega = ?
plot(x = times/365, y = pop.growth.rate(t = times, A = 20, omega = (2/365)*pi.val, 
    theta = 0), type = 'l', ylab = "r")

# for yearly time step, omega = 2*pi
times <- seq(0, 5, 0.002739726) 
plot(x = times, y = pop.growth.rate(t = times, A = 20, omega = 2*pi.val, 
    theta = 0), type = 'l', ylab = "r")

# same...
plot(x = pop.growth.rate(t = seq(0, 5, 0.002739726) , A = 20, omega = 2*pi.val, 
        theta = 0), 
    y = pop.growth.rate(t = seq(0, 5*365, 1), A = 20, omega = (2/365)*pi.val, 
        theta = 0), xlab = "years", ylab = "days", pch = 19, cex = 0.2)
abline(a = 0, b = 1, col = "red")


# Trial population model
#####################################################
rhs.year = function(t, Y, parms){
    with(as.list(c(Y, parms)), {
        # Mosquito - Adults
        r <- pop.growth.rate(t, A = A, omega = 2*pi.val, theta = 0)
        dNvdt = r * Nv
        out <- list(c(Nv = dNvdt))
        return(out)
    })
}
rhs.day = function(t, Y, parms){
    with(as.list(c(Y, parms)), {
        # Mosquito - Adults
        # r = pop.growth.rate(t, A = A, omega = (2*pi.val)/365, theta = 0)
        r <- A * cos(2 * pi.val * t / 365)
        dNvdt = r * Nv
        out <- list(c(Nv = dNvdt))
        return(out)
    })
}

par(mfrow = c(1, 2))
Y0 <- c(Nv = 20)
# for yearly time step, omega = 2*pi
times.year <- seq(0, 5, 0.01)
sol.year <- as.data.frame(ode(Y0, times.year, rhs.year, list(A = 10), method = "ode45"))
plot(x = sol.year$time, y = sol.year$Nv, type = 'l', ylab = "Number of mosquitoes", 
     xlab = "time (years)")

# for daily time step, omega = 2*pi/365
Y0 <- c(Nv = 20)
times.day <- seq(0, 1825, 20) # for daily time step, omega = 2*pi / 365
sol.day <- as.data.frame(ode(Y0, times.day, rhs.day, list(A = 10))) # , rtol = 1e-7, atol = 1e7)
plot(x = sol.day$time, y = sol.day$Nv, 
     type = 'l', ylab = "Number of mosquitoes", xlab = "time (days)")


times <- times.year
rhs <- rhs.year

# Generate three 'typical' mosquito seasonality curves, by changing Y0 and A
plot_curve = function(Aval, Nv0){
    Y0 <- c(Nv = Nv0)
    times <- seq(0, 365*5, 0.1) # daily time-step
    # times <- seq(0, 10, 0.001) # yearly time-step
    sol <- as.data.frame(ode(Y0, times, rhs, list(A = Aval)))
    # sol <- sol[sol$time > 6.6 & sol$time < 9.6,]
    plot(x = sol$time, y = sol$Nv, type = 'l', ylab = "Number of mosquitoes", 
         main = paste(Aval, Nv0, sep = ","), xlab = "time")
}

gen_data = function(Aval, Nv0, name){
    name <- as.character(name)
    Y0 = c(Nv = Nv0)
    # times = seq(0, 365*10, 0.001)
    times = seq(0, 10, 0.001)
    sol <- as.data.frame(ode(Y0, times, rhs, list(A = Aval)))
    sol <- sol[sol$time > 6.75 & sol$time < 9.75,]
    sol$name <- name
    return(sol)
}

# Overall
par(mfrow = c(2, 2)) 
plot_curve(10, 20) # small populations and peaked
plot_curve(20, 20) # large populations and peaked
plot_curve(2, 50) # small populations and less peaked... increase Y0, and turn down amplitude
plot_curve(2, 250) # large populations and less peaked

# Get dataset for ggplot
df <- gen_data(10, 20, "small, peaked")
df <- rbind(df, gen_data(20, 20, "large, peaked"), 
            gen_data(2, 50, "small, less peaked"), 
            gen_data(2, 250, "large, less peaked"))
df$time <- df$time - 5.75
ggplot(df, aes(x = time, y = Nv, colour = name)) + geom_line(lwd = 1.5) + 
    ylab("Number of mosquitoes") + theme_classic() + 
    theme(legend.position = "none",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12))


# For grant: Box 1
df <- gen_data(10, 20, "small, peaked")
df$time <- df$time - 5.75

df2 <- gen_data(20, 20, "large, peaked")
df2$time <- df2$time - 5.75

small <- ggplot(df, aes(x = time, y = Nv), colour = "black") + geom_line(lwd = 1.5) + 
    ylab("Number of mosquitoes") + theme_classic() + ylim(0, 500) +
    theme(legend.position = "none",
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), 
          axis.text.x = element_blank())
large <- ggplot(df2, aes(x = time, y = Nv), colour = "black") + geom_line(lwd = 1.5) + 
    ylab("") + theme_classic() + ylim(0, 500) +
    theme(legend.position = "none",
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), 
          axis.text.x = element_blank())
multiplot(small, large, cols = 2) # abundance only; 700*300


# Box 2: vector competence
df <- gen_data(10, 20, "low")
df <- rbind(df, gen_data(15, 20, "medium"), gen_data(20, 20, "high"))
colnames(df)[3] <- "competence"
df$time <- df$time - 5.75
df$competence2 <- factor(df$competence, levels = c("low", "medium", "high"), ordered = TRUE)

p1 <- ggplot(df, aes(x = time, y = Nv, colour = competence2)) + geom_line(lwd = 1.5) + 
    ylab("Number of mosquitoes") + theme_classic() + ylim(0, 500) +
    scale_colour_manual(values = c("#B71C1C", "#E53935", "#E57373")) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), axis.text.x = element_blank())
p2 <- ggplot(df, aes(x = time, y = Nv, colour = competence2)) + geom_line(lwd = 1.5) + 
    ylab("") + theme_classic() + ylim(0, 500) +
    scale_colour_manual(values = c("#E57373", "#E53935", "#B71C1C")) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), 
          axis.text.x = element_blank())
multiplot(p1, p2, cols = 2) # competence; 700*300

# with legend
ggplot(df, aes(x = time, y = Nv, colour = competence2)) + geom_line(lwd = 1.5) + 
    ylab("") + theme_classic() + ylim(0, 500) +
    scale_colour_manual(values = c("#E57373", "#E53935", "#B71C1C")) + 
    theme(legend.position = "top", 
          legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), 
          axis.text.x = element_blank())

# Box 3: Vertical transmission
df <- gen_data(10, 20, "low")
df <- rbind(df, gen_data(15, 20, "medium"), gen_data(20, 20, "high"))
colnames(df)[3] <- "competence"
df$comp <- factor(df$competence, levels = c("low", "medium", "high"), 
                         ordered = TRUE)
df$time <- df$time - 5.75
df$vertical <- 1
df$vertical[df$competence == "low"] <- 2
df$vertical <- as.factor(df$vertical)
p1 <- ggplot(df, aes(x = time, y = Nv, colour = comp))+ 
    geom_line(aes(linetype = vertical), lwd = 1.5) + 
    ylab("Number of mosquitoes") + theme_classic() + ylim(0, 500) +
    scale_colour_manual(values = c("#B71C1C", "#E53935", "#E57373")) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), axis.text.x = element_blank())

df$vertical <- 1
df$vertical[df$competence %in% c("high", "medium")] <- 2
df$vertical <- as.factor(df$vertical)
p2 <- ggplot(df, aes(x = time, y = Nv, colour = comp))+ 
    geom_line(aes(linetype = vertical), lwd = 1.5) + 
    ylab("") + theme_classic() + ylim(0, 500) +
    scale_colour_manual(values = c("#E57373", "#E53935", "#B71C1C")) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16), axis.text.x = element_blank())
multiplot(p1, p2, cols = 2) # vertical; 700*300
