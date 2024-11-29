#Discrete Time Models Group Project
#Flemy Kabalisa - 121580
#Kenneth Kiprotich - 
#Cynthia Kiambi - 

# Microsoft Options & Stock - 22 Nov 2024 to 21 Feb 2025
r<-6 ; so <- 417; days <-91

require(stats);require(fBasics)
library(stats)
library(fBasics)

#Collecting Our Data

setwd("C:\\Users\\Flemy\\Documents\\Master's Degree\\Semester 1\\Discrete Time Models\\Group Project")

Stock<-read.csv("MSFT Stock Prices - 2018-2024.csv")
Opt<-read.csv("MSFT Option Prices - Feb '25.csv")

Opt <- Opt[-1,]

#Data Analysis

daat <- diff(log(Stock[,5]))
basicStats(daat); dam<-daat


#--------- Question 3 Parts a & b ----------#


# Function to generate the stock price tree
stock_tree <- function(s, delta, u, d, N) {
  tree <- matrix(0, nrow = N + 1, ncol = N + 1)
  for (i in 1:(N + 1)) {
    for (j in 1:i) {
      tree[i, j] = s * (u^(j - 1) * d^((i - 1) - (j - 1)))
    }
  }
  return(tree)
}

# Function for pricing European options
CRR_call_put <- function(tree, u, d, delta_t, r, K, type) {
  R = 1 + (r * 0.01 * delta_t)
  q = (R - d) / (u - d)
  option_tree = matrix(0, nrow = nrow(tree), ncol = ncol(tree))
  
  # Payoff at maturity
  if (type == "put") {
    option_tree[nrow(option_tree), ] = pmax(K - tree[nrow(tree), ], 0)
  } else {
    option_tree[nrow(option_tree), ] = pmax(tree[nrow(tree), ] - K, 0)
  }
  
  # Backward induction
  for (i in (nrow(tree) - 1):1) {
    for (j in 1:i) {
      option_tree[i, j] = ((1 - q) * option_tree[i + 1, j] + q * option_tree[i + 1, j + 1]) / R
    }
  }
  return(option_tree[1, 1])
}


#Parameters
s = 417           # Initial stock price
delta = 1/365     # Time step
u = as.numeric(exp(sd(daat)))      # Up factor
d = 1 / u        # Down factor
r = 6           # Risk-free interest rate (in percentage)
N = 91           # Fixed number of steps From November 22, 2024 to February 21, 2025
strike <- Opt[,7] #Extract the Strike Prices used
strike_prices = as.numeric(strike)  # Vector of strike prices


# Generate the stock price tree
trees <- stock_tree(s, delta, u, d, N)

# Data frame to store results
results <- data.frame(
  Strike_Price = numeric(),
  European_Call = numeric(),
  European_Put = numeric()
)

# Iterate over the strike prices and calculate option prices
for (K in strike_prices) {
  european_call = CRR_call_put(trees, u, d, delta_t = delta, r = r, K = K, type = "call")
  european_put = CRR_call_put(trees, u, d, delta_t = delta, r = r, K = K, type = "put")

  # Append results to the data frame
  results <- rbind(results, data.frame(
    Strike_Price = K,
    European_Call = round(european_call, 2),
    European_Put = round(european_put, 2)
  ))
}
results
# Display the results as a table
if (!require(knitr)) install.packages("knitr")
library(knitr)

results_final <-kable(results, 
      col.names = c("Strike Price", "European Call", "European Put"),
      caption = "Miscosoft Option Prices Maturing on 21 Feb 2025",
      align = 'c')

results_final
