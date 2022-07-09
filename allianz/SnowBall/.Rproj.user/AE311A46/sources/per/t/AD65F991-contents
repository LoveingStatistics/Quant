# index <- list(S0 = 1,
#               sigma = 0.13,
#               knock_in = 0.85,
#               knock_out = 1.03,
#               r = 0.03,
#               pay = 0.2,
#               NT = 252) #parameters needed
#names(index) <- c('S0','sigma','knock_in','knock_out','r','pay','NT')

#N <- 300000 #total number of simulations

# compress_nav <- function(nav) {
#
#   dr <- nav / shift(nav) - 1
#   dr[is.na(dr)] <- 0
#   compress_dr <- dr / sqrt(252)
#   compress_nav <- nav[1] * cumprod(1 + compress_dr)
# }


# calculate_s_t <- function(index, st = 1, t = 0){#calculate underlying price
#
#   s0 <- copy(index[["S0"]])
#   sigma <- copy(index[["sigma"]])
#   knock_in <- copy(index[["knock_in"]])
#   knock_out <- copy(index[["knock_out"]])
#   r <- copy(index[["r"]])
#   pay <- copy(index[["pay"]])
#   NT <- copy(index[["NT"]])
#
#   epsilon <- rnorm(NT - t) #random factor
#   Time <- (1:(NT - t)) / 252 #time sequence
#
#   if(t == 0){
#     St <- s0 * exp((r - 0.5 * sigma^2) * Time + sigma * epsilon * sqrt(Time)) #simulated underlying price
#     St <- c(s0, St)#include S0
#     #St <- compress_nav(St)
#   }
#   else{
#     St <- st * exp((r - 0.5 * sigma^2) * Time + sigma * epsilon * sqrt(Time))#simulated underlying price
#     #St <- compress_nav(St)
#     St <- c(rep(st, t + 1), St)
#   }
#   return(St)
# }


calculate_s_t <- function(index, st = 1, t = 0, epsilon){#calculate underlying price

  s0 <- copy(index[["S0"]])
  sigma <- copy(index[["sigma"]])
  knock_in <- copy(index[["knock_in"]])
  knock_out <- copy(index[["knock_out"]])
  r <- copy(index[["r"]])
  pay <- copy(index[["pay"]])
  NT <- copy(index[["NT"]])

  #epsilon <- rnorm(NT - t) #random factor

  dr <- exp((r - 0.5 * sigma ^ 2) / 252 + sigma * epsilon / sqrt(252)) - 1
  St <- st * cumprod(1 + dr)
  St <- c(rep(st, t + 1), St)
  return(St)
}


payoff_beta <- function(St, index, t, knock_in_not){#estimate the value of snowball option

  s0 <- copy(index[["S0"]])
  sigma <- copy(index[["sigma"]])
  knock_in <- copy(index[["knock_in"]])
  knock_out <- copy(index[["knock_out"]])
  r <- copy(index[["r"]])
  pay <- copy(index[["pay"]])
  NT <- copy(index[["NT"]])

  #knock_out_obs <- seq(0,252,length.out=13)[-1] - t #knock-out observation
  knock_out_obs <- seq(0, NT, length.out = NT / 252 * 12 + 1)[-1]
  knock_out_obs <- knock_out_obs[knock_out_obs >= t]
  knock_out_pt <- knock_out_obs[St[knock_out_obs] > knock_out]#knock-out


  if(length(knock_out_pt) != 0){#calculate knock-out discounted profit
    gain <- pay * head(knock_out_pt, 1) / 252  #持有期收益
    discount <- gain * exp(-r * (head(knock_out_pt, 1) - t) / 252)
    holding <- head(knock_out_pt, 1)
  } else{
    if(knock_in_not == 1){#之前时点就已经有敲入过，那么直接根据期末值进行计算即可
      if (tail(St, 1) >= s0) {
        gain <- 0
      } else {
        gain <- tail(St, 1) - s0
      }
      discount <- gain * exp(-r * (NT - t) / 252)
    } else{#否则说明之前时点没有过敲入，那么就判断之后有没有敲入
      knock_in_pt <- which(St < knock_in) #knock-in points
      knock_in_pt <- knock_in_pt[knock_in_pt >= t]
      if(length(knock_in_pt) != 0){#当前时点之后在未来有敲入点
        if(tail(St, 1) >= s0){
          gain <- 0 #这里和产品设计有区别
        } else{#calculate knock-in and suffer loss discounted value
          gain <- tail(St, 1) - s0
        }
        discount <- gain * exp(-r * (NT - t) / 252)
      } else{#calculate neither knock-in nor knock-out discounted value
        gain <- pay * NT / 252  #持有期收益
        discount <- gain * exp(-r * (NT - t) / 252)
      }
    }
    holding <- NT
  }
  res <- list(GAIN = gain,
              DISCOUNT = discount,
              HOLDING = holding)
}

# index <- list(S0 = 1,
#               sigma = 0.13,
#               knock_in = 0.85,
#               knock_out = 1.03,
#               r = 0.03,
#               pay = 0.2,
#               NT = 252) #parameters needed
snowball_npv <- function(st, t, index, knock_in_out, N = 50000) {

  npvs <- vector()
  for (i in 1 : N) {
    NT <- copy(index[["NT"]])
    epsilon <- rnorm(NT - t) #random factor
    St <- calculate_s_t(index, st, t, epsilon)
    npv <- payoff_beta(St, index, t, knock_in_out)$DISCOUNT
    npvs <- c(npvs, npv)
  }
  mean_npvs <- mean(npvs)
}


snowball_delta <- function(st, t, index, knock_in_out, N = 50000) {

  st_up <- st + st / 100
  st_down <- st - st / 100

  up_npvs <- vector()
  down_npvs <- vector()
  for (i in 1 : N) {
    NT <- copy(index[["NT"]])
    epsilon <- rnorm(NT - t) #random factor

    St_up <- calculate_s_t(index, st_up, t, epsilon)
    up_npv <- payoff_beta(St_up, index, t, knock_in_out)$DISCOUNT
    up_npvs <- c(up_npvs, up_npv)

    St_down <- calculate_s_t(index, st_down, t, epsilon)
    down_npv <- payoff_beta(St_down, index, t, knock_in_out)$DISCOUNT
    down_npvs <- c(down_npvs, down_npv)
  }
  mean_up_npv <- mean(up_npvs)
  mean_down_npv <- mean(down_npvs)

  mean_up_npv <- round(mean_up_npv, 6)
  mean_down_npv <- round(mean_down_npv, 6)

  delta <- (mean_up_npv - mean_down_npv) / (st_up - st_down)
}

# test
# snowball_npvs <- snowball_npv(st = 1, t = 0, index, knock_in_out = 0)
# mean_npvs <- mean(snowball_npvs)

