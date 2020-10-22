# plotting functions
plot_vectors = function(sol, time.subset = sol$time){
    par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0))
    df <- sol[sol$time %in% time.subset, ]
    if (length(sol) == 10) {
        df$Ev <- df$Ev1 + df$Ev2 + df$Ev3
        df$Sv <- df$Nv - df$Ev - df$Iv    
        plot(x = df$time, y = df$Nv, type = 'l', ylab = "Nv", xlab = "time")
        plot(x = df$time, y = df$Sv, type = 'l', ylab = "Sv", xlab = "time")
        plot(x = df$time, y = df$Ev, type = 'l', ylab = "Ev", xlab = "time")
        plot(x = df$time, y = df$Iv, type = 'l', ylab = "Iv", xlab = "time")
    } else if (length(sol) == 15) {
        df$E1v <- df$E1v1 + df$E1v2 + df$E1v3 
        df$E2v <- df$E2v1 + df$E2v2 + df$E2v3
        df$S1v <- df$N1v - df$E1v - df$I1v
        df$S2v <- df$N2v - df$E2v - df$I2v
        nlim = c(min(df$N1v, df$N2v) - 5, max(df$N1v, df$N2v) + 5)
        slim = c(min(df$S1v, df$S2v) - 5, max(df$S1v, df$S2v) + 5)
        elim = c(min(df$E1v, df$E2v) - 5, max(df$E1v, df$E2v) + 5)
        ilim = c(min(df$I1v, df$I2v) - 5, max(df$I1v, df$I2v) + 5)
        plot(x = df$time, y = df$N1v, type = 'l', ylab = "Nv", xlab = "time", 
             ylim = nlim, las = 1)
        lines(x = df$time, y = df$N2v, col = "blue")
        plot(x = df$time, y = df$S1v, type = 'l', ylab = "Sv", xlab = "time", 
             ylim = slim, las = 1)
        lines(x = df$time, y = df$S2v, col = "blue")
        plot(x = df$time, y = df$E1v, type = 'l', ylab = "Ev", xlab = "time",
             ylim = elim, las = 1)
        lines(x = df$time, y = df$E2v, col = "blue")
        plot(x = df$time, y = df$I1v, type = 'l', ylab = "Iv", xlab = "time", 
             ylim = ilim, las = 1)
        lines(x = df$time, y = df$I2v, col = "blue")
    } else {
        df$E1v <- df$E1v1 + df$E1v2 + df$E1v3 
        df$E2v <- df$E2v1 + df$E2v2 + df$E2v3
        df$E3v <- df$E3v1 + df$E3v2 + df$E3v3
        df$S1v <- df$N1v - df$E1v - df$I1v
        df$S2v <- df$N2v - df$E2v - df$I2v
        df$S3v <- df$N3v - df$E3v - df$I3v
        nlim = c(min(df$N1v, df$N2v, df$N3v)- 5, max(df$N1v, df$N2v, df$N3v)+ 5)
        slim = c(min(df$S1v, df$S2v, df$S3v)- 5, max(df$S1v, df$S2v, df$S3v)+ 5)
        elim = c(min(df$E1v, df$E2v, df$E3v)- 5, max(df$E1v, df$E2v, df$E3v)+ 5)
        ilim = c(min(df$I1v, df$I2v, df$I3v)- 5, max(df$I1v, df$I2v, df$I2v)+ 5)
        plot(x = df$time, y = df$N1v, type = 'l', ylab = "Nv", xlab = "time", 
             ylim = nlim, las = 1)
        lines(x = df$time, y = df$N2v, col = "blue")
        lines(x = df$time, y = df$N3v, col = "green")
        plot(x = df$time, y = df$S1v, type = 'l', ylab = "Sv", xlab = "time", 
             ylim = slim, las = 1)
        lines(x = df$time, y = df$S2v, col = "blue")
        lines(x = df$time, y = df$S3v, col = "green")
        plot(x = df$time, y = df$E1v, type = 'l', ylab = "Ev", xlab = "time",
             ylim = elim, las = 1)
        lines(x = df$time, y = df$E2v, col = "blue")
        lines(x = df$time, y = df$E3v, col = "green")
        plot(x = df$time, y = df$I1v, type = 'l', ylab = "Iv", xlab = "time", 
             ylim = ilim, las = 1)
        lines(x = df$time, y = df$I2v, col = "blue")
        lines(x = df$time, y = df$I3v, col = "blue")
    }
}

plot_hosts = function(sol, time.subset = sol$time){
    par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0))
    df <- sol[sol$time %in% time.subset, ]
    plot(x = df$time, y = df$Sh, type = 'l', ylab = "Sh", xlab = "time")
    plot(x = df$time, y = df$Eh, type = 'l', ylab = "Eh", xlab = "time")
    plot(x = df$time, y = df$Ih, type = 'l', ylab = "Ih", xlab = "time")
    plot(x = df$time, y = df$Rh, type = 'l', ylab = "Rh", xlab = "time")
}