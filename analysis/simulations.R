


run_many_sims <- function(B = 20, p = 20, deg = 3,
                          n = 250, nval = 250, 
                          sigma = 10, sigmay = 1,
                          n.ids = 10, each.id = 25,
                          yrange = c(10,80)){
  out_gbm <- out_gbmalign <-
    out_pc <- out_pcalign <- 
    out_las <- out_lasalign <- 
    out_longlas <- out_longlasalign <- 
    c()
  
  
  for(b in 1:B){
    cat(" ", b)
    id <- rep(1:n.ids, each = each.id)
    
    truey <- y <- gap <- rep(NA, n)
    
    for(i in 1:n.ids){
      truey[id == i] <- runif(each.id)*(yrange[2] - yrange[1]) + yrange[1]
      
      y[id == i] <- truey[id==i] + sigmay*rnorm(1)
      
      gap[id == i] <- y[id==i] - min(y[id == i])
    }
    
    
    
    Amat <- matrix(rnorm(deg*p), nrow = deg)
    
    x <- poly( (y - mean(y))/sd(y), deg, raw = TRUE, simple = TRUE) %*% Amat
    
    # x <- splines::bs(y, df = df, degree = 4) %*% Amat
    # 
    # cols <- sample(1:ncol(x), p)
    # 
    # x <- x[, cols]
    # x <- scale(x)
    
    x <- x + sigma*matrix(rnorm(length(x)), nrow = nrow(x))
    
    
    nval.ids <- 10
    
    idval <- rep(1:nval.ids, each = each.id)
    trueyval <- yval <- gapval <- rep(NA, nval)
    
    for(i in 1:nval.ids){
      trueyval[idval == i] <- runif(each.id)*(yrange[2] - yrange[1]) + yrange[1]
      
      yval[id==i] <- trueyval[id==i] + sigmay*rnorm(1)
      
      gapval[idval == i] <- yval[idval==i] - min(yval[idval == i])
    }
    
    
    xval <- poly( (yval - mean(yval))/sd(yval), deg, raw = TRUE, simple = TRUE) %*% Amat
    
    xval <- xval + sigma*matrix(rnorm(length(xval)), nrow = nrow(xval))
    
    
    
    fit <- glmnet::cv.glmnet(x, y)
    
    yhat <- predict(fit, xval, s = "lambda.1se")
    
    cat(".")
    
    out_las <- append(out_las, abs(yhat - yval))
    
    yhat2 <- alignment_response(yhat, gapval, idval)
    out_lasalign <- append(out_lasalign, abs(yhat2 - yval))
    
    fitgbm <- gest_fit_gbm(x, y, gap, id)
    yhatgbm <- predict.gest_fit_gbm(fitgbm, xval, gapval, idval)
    out_gbm <- append(out_gbm, abs(yhatgbm - yval))
    
    cat(".")
    
    yhatgbm2 <- alignment_response(yhatgbm, gapval, idval)
    out_gbmalign <- append(out_gbmalign, abs(yhatgbm2 - yval))
    
    suppressMessages({
      fitlonglas <- gest_fit_longlas(x, y, gap, id)
      yhatlonglas <- predict.gest_fit_longlas(fitlonglas, xval, gapval, idval)
      out_longlas <- append(out_longlas, abs(yhatlonglas - yval))
    })
    cat(".")
    
    yhatlonglas2 <- alignment_response(yhatlonglas, gapval, idval)
    out_longlasalign <- append(out_longlasalign, abs(yhatlonglas2 - yval))
    
    
    
    fitpc <- gest_fit_pcurve(x, y, gap, id, 
                             Boundary.knots = yrange + c(-1,1),
                             gage.grid = seq(from = yrange[1], to = yrange[2], by = .1),
                             knots = seq(from = yrange[1], to = yrange[2], by = 10))
    
    yhatpc <- predict.gest_fit_pcurve(fitpc, xval, gapval, idval)
    out_pc <- append(out_pc, abs(yhatpc - yval))
    
    cat(".")
    
    yhatpc2 <- alignment_response(yhatpc, gapval, idval)
    out_pcalign <- append(out_pcalign, abs(yhatpc2 - yval))
  }
  
  ret <- data.frame(out_gbm, out_gbmalign,
                    out_las, out_lasalign,
                    out_longlas, out_longlasalign,
                    out_pc, out_pcalign,
                    p = sprintf("p = %d", p),
                    deg = sprintf("deg = %d", deg),
                    sigma = sprintf("sigma = %.1f", sigma),
                    sigmay = sprintf("sigmay = %.1f", sigmay)) 
}


out1 <- run_many_sims(p = 10)
out2 <- run_many_sims(p = 100)
out3 <- run_many_sims(p = 1000)

out1b <- run_many_sims(p = 10, sigma = .1)
out2b <- run_many_sims(p = 100, sigma = .1)
out3b <- run_many_sims(p = 1000, sigma = .1)

out1a <- run_many_sims(p = 10, sigma = 1)
out2a <- run_many_sims(p = 100, sigma = 1)
out3a <- run_many_sims(p = 1000, sigma = 1)

out <- bind_rows(out1, out2, out3,
                 out1a, out2a, out3a,
                 out1b, out2b, out3b)

save(out, file = "simulations_data.RData")


p <- out %>%
  pivot_longer(cols = starts_with("out_")) %>%
  mutate(model = str_remove_all(name, "out_|align"),
         aligned = ifelse(str_detect(name, "align"), "Aligned", "No Alignment")) %>%
  filter(! ((model=="pc") & aligned == "Aligned")) %>%
  filter(! ((model == "longlas") & aligned == "Aligned")) %>%
  mutate(aligned = ifelse(model == "pc", "Aligned", aligned)) %>%
  ggplot() +
  geom_boxplot(aes(y = value, x=model, colour = aligned), width = 0.5) + 
  scale_colour_manual(breaks = c("No Alignment", "Aligned"), 
                      values = c("black", "red"),
                      name = "") +
  theme_bw() +
  facet_wrap(~p*sigma, scales = "free_y") +
  scale_x_discrete(name = "", 
                   breaks = c("gbm", "las", "longlas", "pc"),
                   labels = c("Boosted\nTrees", "LASSO", "Longit.\nLASSO", "Principal\nCurves")) + 
  labs(y = "Absolute Prediction Errors") + 
  theme(legend.position = "bottom")

ggsave(p, file = "simulations.png", width = 8, height = 9)
