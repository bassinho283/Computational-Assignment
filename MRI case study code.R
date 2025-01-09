library(ggplot2)


data <- read.csv('ScanRecords.csv')
data_type1 <- data[data$PatientType == 'Type 1', ]
data_type2 <- data[data$PatientType == 'Type 2', ]


#TYPE I PATIENTS

#Scan duration - Type I patients
#Type I patients: we know durations are normally distributed, we do not know the mean and sd.
#Mean and sd of scan durations type 1 patients:
mean_scanduration_type1 <- mean(data_type1$Duration)
cat('The mean scan duration for type I patients:', mean_scanduration_type1, '\n')
sd_scanduration_type1 <- sd(data_type1$Duration)
cat('The standard deviation of the scan duration for type I patients:', sd_scanduration_type1, '\n')
#Making a histogram (+normal curve)
hist(data_type1$Duration, probability = TRUE, main = 'Histogram of scan duration for type I patients',
     xlab = 'Duration (hours)', col = 'yellow2')
curve(dnorm(x, mean = mean_scanduration_type1, sd = sd_scanduration_type1), 
      col = 'red', lwd = 4, add = TRUE)

#Arrival times - Type I patients
#Type I patients: we know that the number of patients per day is distributed according to a Poisson distribution. 
#We know that the inter-arrival time is exponentially distributed.
#Mean of the (inter-)arrival time distribution is unknown.

#Arrival times
patients_per_day <- table(as.Date(data_type1$Date))
mean_patients_per_day <- mean(patients_per_day)
poisson_lambda <- mean_patients_per_day
cat('The Poisson parameter for arrival times of type I patients:', poisson_lambda, '\n')
#Making a histogram (+Poisson curve)
hist(patients_per_day, breaks = seq(min(patients_per_day), max(patients_per_day)+1, by = 1), probability = TRUE, main = 'Histogram of number of arrivals per day for type I patients', 
     xlab = 'Number of patients per day', col = 'orange')
patient_numbers <- seq(min(patients_per_day), max(patients_per_day), by = 1)
lines(patient_numbers, dpois(patient_numbers, lambda = poisson_lambda), col = 'cyan2', lwd = 4)

#Interarrival times
arrival_times <- numeric(length(data_type1$Time) - 1)
for (i in 2:length(data_type1$Time)) {
  #Same day
  if (data_type1$Date[i] == data_type1$Date[i - 1]) {
    arrival_times[i - 1] <- data_type1$Time[i] - data_type1$Time[i - 1]
  } else {
    #One day difference
    end_of_day <- 17  
    start_of_day <- 8
    time_diff_within_day <- end_of_day - data_type1$Time[i - 1]
    time_diff_next_day <- data_type1$Time[i] - start_of_day
    arrival_times[i - 1] <- (end_of_day - data_type1$Time[i - 1]) + (data_type1$Time[i] - start_of_day)
  }
}
mean_arrival_times <- mean(arrival_times)
exponential_lambda <- 1 / mean_arrival_times
cat('The exponential parameter for inter-arrival times for type I patients:', exponential_lambda, "\n")
#Making the histogram (+exponential curve)
hist(arrival_times, probability = TRUE, breaks = 25, main = 'Histogram of inter-arrival times for type I patients)', 
     xlab = 'Inter-arrival times (hours)', col = 'violet')
interarrivaltime_numbers <- seq(0, max(arrival_times), length.out = 100)
lines(interarrivaltime_numbers, dexp(interarrivaltime_numbers, rate = exponential_lambda), col = 'red', lwd = 4)

#Bootstrap #1 - Scan duration (mean and standard deviation)
scandurations_type1 <- data_type1$Duration
scandurations_type1_mean <- mean(scandurations_type1)
scandurations_type1_sd <- sd(scandurations_type1)
set.seed(123)
R <- 1000
bootstrap_means <- numeric(R)
bootstrap_sds <- numeric(R)
for (i in 1:R) {
  resample <- sample(scandurations_type1, size = length(scandurations_type1), replace = TRUE)
  bootstrap_means[i] <- mean(resample)
  bootstrap_sds[i] <- sd(resample)
}
mean_bootstrap_ci <- quantile(bootstrap_means, probs = c(0.025, 0.975))
sd_bootstrap_ci <- quantile(bootstrap_sds, probs = c(0.025, 0.975))
cat('Mean of the resample with a 95% CI:', mean_bootstrap_ci, '\n')
cat('Standard deviation of the resample with a 95% CI:', sd_bootstrap_ci, '\n')

if (scandurations_type1_mean >= mean_bootstrap_ci[1] && scandurations_type1_mean <= mean_bootstrap_ci[2]) {
  cat('The estimated mean is validated by bootstrap results.')
} else {
  cat('The estimated mean is NOT validated by bootstrap results.')
}

if (scandurations_type1_sd >= sd_bootstrap_ci[1] && scandurations_type1_sd <= sd_bootstrap_ci[2]) {
  cat('The estimated standard deviation is validated by bootstrap results.')
} else {
  cat('The estimated standard deviation is NOT validated by bootstrap results.')
}


#Bootstrap #2 - Arrival times (lambda for Poisson distribution)
set.seed(123)
R <- 1000
bootstrap_poisson_lambdas <- numeric(R)
for (i in 1:R) {
  resample2 <- sample(patients_per_day, size = length(patients_per_day), replace = TRUE)
  bootstrap_poisson_lambdas[i] <- mean(resample2)
}
bootstrap_poisson_lambda_ci <- quantile(bootstrap_poisson_lambdas, probs = c(0.025, 0.975))
cat('Lambda of the resample with a 95% CI:', bootstrap_poisson_lambda_ci, '\n')

if (poisson_lambda >= bootstrap_poisson_lambda_ci[1] && poisson_lambda <= bootstrap_poisson_lambda_ci[2]) {
  print('The estimated distribution parameter is validated by bootstrap results.')
} else {
  print('The estimated distribution parameter is NOT validated by bootstrap results.')
}


#Bootstrap #3 - Inter-arrival times (lambda for exponential distribution)
set.seed(123)
R <- 1000
bootstrap_exponential_lambdas <- numeric(R)
for (i in 1:R) {
  resample3 <- sample(arrival_times, size = length(arrival_times), replace = TRUE)
  bootstrap_exponential_lambdas[i] <- 1 / mean(resample3)
}
bootstrap_exponential_lambda_ci <- quantile(bootstrap_exponential_lambdas, probs = c(0.025, 0.975))
cat('Lambda of the resample with a 95% CI:', bootstrap_exponential_lambda_ci, '\n')

if (exponential_lambda >= bootstrap_exponential_lambda_ci[1] && exponential_lambda <= bootstrap_exponential_lambda_ci[2]) {
  print('The estimated distribution parameter is validated by bootstrap results.')
} else {
  print('The estimated distribution parameter is NOT validated by bootstrap results.')
}

#Code for histograms used in report

hist(data_type1$Duration, probability = TRUE, main = 'Histogram of scan duration for type I patients',
     xlab = 'Duration (hours)', col = 'yellow2')
curve(dnorm(x, mean = mean_scanduration_type1, sd = sd_scanduration_type1), 
      col = 'red', lwd = 4, add = TRUE)
abline(v = mean(scandurations_type1), col = 'darkgreen', lwd = 4)  
abline(v = mean_bootstrap_ci, col = 'blue', lwd = 4, lty = 2)  
legend('topright', legend = c('Normal curve', 'Bootstrap CI', 'Sample mean'), col = c('red', 'blue', 'darkgreen'), lty = c(1, 2), bty = 'n')

hist(patients_per_day, breaks = seq(min(patients_per_day), max(patients_per_day) + 1, by = 1), 
     probability = TRUE, main = 'Histogram of number of arrivals per day for type I patients', 
     xlab = 'Number of patients per day', col = 'peachpuff2')
lines(patient_numbers, dpois(patient_numbers, lambda = poisson_lambda), col = 'cyan2', lwd = 4)
abline(v = mean(bootstrap_poisson_lambdas), col = 'darkgreen', lwd = 4)  
abline(v = bootstrap_poisson_lambda_ci, col = 'blue', lwd = 4, lty = 2)  
legend('topright', legend = c('Poisson curve', 'Bootstrap CI', 'Sample mean'), col = c('cyan', 'blue', 'darkgreen'), lty = c(1, 2), bty = 'n')

hist(arrival_times, probability = TRUE, breaks = 25, main = 'Histogram of inter-arrival times for type I patients', 
     xlab = 'Inter-arrival times (hours)', col = 'grey')
lines(interarrivaltime_numbers, dexp(interarrivaltime_numbers, rate = exponential_lambda), col = 'purple',lwd = 4)
abline(v = mean(bootstrap_exponential_lambdas), col = 'darkgreen', lwd = 4)  # Mean as solid green line
abline(v = bootstrap_exponential_lambda_ci, col = 'blue', lwd = 4, lty = 2)  # CI as dashed blue lines
legend('topright', legend = c('Exponential curve', 'Bootstrap CI', 'Sample mean'), col = c('purple', 'blue', 'darkgreen'), lty = c(1,2), bty = 'n')


#TYPE II PATIENTS

#Scan duration - Type II patients
#Type II patients: we do not know the distribution of the scan duration.
mean_scanduration_type2 <- mean(data_type2$Duration)
cat('The mean scan duration for type II patients:', mean_scanduration_type2, '\n')
sd_scanduration_type2 <- sd(data_type2$Duration)
cat('The standard deviation of the scan duration for type II patients:', sd_scanduration_type2, '\n')
#Making a histogram
hist(data_type2$Duration, probability = TRUE, main = 'Histogram of scan duration for type II patients',
     xlab = 'Duration (hours)', col = 'lightgreen')

#Bootstrap #1 - Scan duration type II patients
set.seed(123)
R <- 1000
bootstrap_means <- numeric(R)
bootstrap_sds <- numeric(R)
for (i in 1:R) {
  resample4 <- sample(data_type2$Duration, size = length(data_type2$Duration), replace = TRUE)
  bootstrap_means[i] <- mean(resample4)
  bootstrap_sds[i] <- sd(resample4)
}
mean_scandurationbootstrap_ci_type2 <- quantile(bootstrap_means, probs = c(0.025, 0.975))
sd_scandurationbootstrap_ci_type2 <- quantile(bootstrap_sds, probs = c(0.025, 0.975))
cat('Mean of the resample with a 95% CI - type II:', mean_scandurationbootstrap_ci_type2, '\n')
cat('Standard deviation of the resample with a 95% CI - type II:', sd_scandurationbootstrap_ci_type2, '\n')


#Arrival times - type II patients
patients_per_day_2 <- table(as.Date(data_type2$Date))
mean_patients_per_day_2 <- mean(patients_per_day_2)
sd_patients_per_day_2 <- sd(patients_per_day_2) 
#Making a histogram
hist(patients_per_day_2, breaks = seq(min(patients_per_day_2), max(patients_per_day_2)+1, by = 1), probability = TRUE, main = 'Histogram of number of arrivals per day for type II patients', 
     xlab = 'Number of patients per day', col = 'pink2')

#Bootstrap #2 - Arrival times type II patients
set.seed(123)
R <- 1000
for (i in 1:R) {
  resample5 <- sample(patients_per_day_2, size = length(patients_per_day_2), replace = TRUE)
  bootstrap_means[i] <- mean(resample5)
  bootstrap_sds[i] <- sd(resample5)
}
mean_arrivaltimebootstrap_ci_type2 <- quantile(bootstrap_means, c(0.025, 0.975)) 
sd_arrivaltimebootstrap_ci_type2 <- quantile(bootstrap_sds, c(0.025, 0.975)) 
cat('Mean of the resample with a 95% CI - type II:', mean_arrivaltimebootstrap_ci_type2, '\n')
cat('Standard deviation of the resample with a 95% CI - type II:',sd_arrivaltimebootstrap_ci_type2, '\n')


#Inter-arrival times - type II patients
arrival_times2 <- numeric(length(data_type2$Time) - 1)
for (i in 2:length(data_type2$Time)) {
  #Same day
  if (data_type2$Date[i] == data_type2$Date[i - 1]) {
    arrival_times2[i - 1] <- data_type2$Time[i] - data_type2$Time[i - 1]
  } else {
    #One day difference
    end_of_day2 <- 17  
    start_of_day2 <- 8
    time_diff_within_day2 <- end_of_day2 - data_type2$Time[i - 1]
    time_diff_next_day2 <- data_type2$Time[i] - start_of_day2
    arrival_times2[i - 1] <- (end_of_day2 - data_type2$Time[i - 1]) + (data_type2$Time[i] - start_of_day2)
  }
}
mean_arrival_times2 <- mean(arrival_times2)
#Making the histogram
hist(arrival_times2, probability = TRUE, breaks = 25, main = 'Histogram of inter-arrival times for type II patients', 
     xlab = 'Inter-arrival times (hours)', col = 'steelblue')
#Bootstrap #3 - Inter-arrival times
set.seed(123)
R <- 1000
for (i in 1:R) {
  resample6 <- sample(arrival_times2, size = length(arrival_times2), replace = TRUE)
  bootstrap_means[i] <- mean(resample6)
  bootstrap_sds[i] <- sd(resample6)
}
mean_interarrivaltimebootstrap_ci_type2 <- quantile(bootstrap_means, c(0.025, 0.975)) 
sd_interarrivaltimebootstrap_ci_type2 <- quantile(bootstrap_sds, c(0.025, 0.975)) 
cat('Mean of the resample with a 95% CI - type II:', mean_interarrivaltimebootstrap_ci_type2, '\n')
cat('Standard deviation of the resample with a 95% CI - type II:',sd_interarrivaltimebootstrap_ci_type2, '\n')


#Monte Carlo for type II patients - Scan duration
mean_scanduration_type2 <- mean(data_type2$Duration)
cat('The observed mean scan duration for type II patients:', mean_scanduration_type2, '\n')
sd_scanduration_type2 <- sd(data_type2$Duration)
cat('The observed standard deviation of the scan duration for type II patients:', sd_scanduration_type2, '\n')
set.seed(123)
n <- length(data_type2$Duration)
R <- 100000
mc_means_scandurations <- numeric(R)
mc_sds_scandurations <- numeric(R)
for (i in 1:R) {
  mc_scandurations <- sample(data_type2$Duration, size = n, replace = TRUE)
  mc_means_scandurations[i] <- mean(mc_scandurations)
  mc_sds_scandurations[i] <- sd(mc_scandurations)
}
mean_mc_scandurations_ci <- quantile(mc_means_scandurations, probs = c(0.025, 0.975))
sd_mc_scandurations_ci <- quantile(mc_sds_scandurations, probs = c(0.025, 0.975))
cat('95% CI for Monte Carlo means (scan durations):', mean_mc_scandurations_ci, '\n')
cat('95% CI for Monte Carlo standard deviations (scan durations):', sd_mc_scandurations_ci, '\n')

#Histogram for simulated means
hist(mc_means_scandurations, probability = TRUE, col = 'salmon2',
     main = 'Simulated means versus observed means of scan duration for type II patients',
     xlab = 'Simulated means')
abline(v = mean_scanduration_type2, col = 'darkblue', lwd = 4)

#Histogram for simulated standard deviations
hist(mc_sds_scandurations, probability = TRUE, col = 'lightgreen',
     main = 'Simulated standard deviations versus observed standard deviation of scan duration for type II patients',
     xlab = 'Simulated standard deviations')
abline(v = sd_scanduration_type2, col = 'purple4', lwd = 4)

#Monte Carlo for type II patients - Arrival times
patients_per_day_2 <- table(as.Date(data_type2$Date))
mean_patients_per_day_2 <- mean(patients_per_day_2)
sd_patients_per_day_2 <- sd(patients_per_day_2) 

set.seed(123)
n <- length(patients_per_day_2)
R <- 100000
mc_means_patientsperday <- numeric(R)
mc_sds_patientsperday <- numeric(R)
for (i in 1:R) {
  mc_patientsperday <- sample(patients_per_day_2, size = n, replace = TRUE)
  mc_means_patientsperday[i] <- mean(mc_patientsperday)
  mc_sds_patientsperday[i] <- sd(mc_patientsperday)
}
mean_mc_patientsperday_ci <- quantile(mc_means_patientsperday, probs = c(0.025, 0.975))
sd_mc_patientsperday_ci <- quantile(mc_sds_patientsperday, probs = c(0.025, 0.975))
cat('95% CI for Monte Carlo means (arrival times):', mean_mc_patientsperday_ci, '\n')
cat('95% CI for Monte Carlo standard deviations (arrival times):', sd_mc_patientsperday_ci, '\n')

#Histogram for simulated means
hist(mc_means_patientsperday, probability = TRUE, col = 'wheat',
     main = 'Simulated means versus observed means of arrival times for type II patients',
     xlab = 'Simulated means')
abline(v = mean_patients_per_day_2, col = 'darkgrey', lwd = 4)

#Histogram for simulated standard deviations
hist(mc_sds_patientsperday, probability = TRUE, col = 'gold',
     main = 'Simulated standard deviations versus observed standard deviation of arrival times for type II patients',
     xlab = 'Simulated standard deviations')
abline(v = sd_patients_per_day_2, col = 'rosybrown', lwd = 4)

#Monte Carlo for type II patients - Inter-arrival times
mean_arrival_times2 <- mean(arrival_times2)
sd_arrival_times2 <- sd(arrival_times2)

set.seed(123)
n <- length(arrival_times2)
R <- 100000  
mc_means_interarrivaltimes <- numeric(R)
mc_sds_interarrivaltimes <- numeric(R)
for (i in 1:R) {
  mc_interarrivaltimes <- sample(arrival_times2, size = n, replace = TRUE)
  mc_means_interarrivaltimes[i] <- mean(mc_interarrivaltimes)
  mc_sds_interarrivaltimes[i] <- sd(mc_interarrivaltimes)
}
mean_mc_interarrivaltimes_ci <- quantile(mc_means_interarrivaltimes, probs = c(0.025, 0.975))
sd_mc_interarrivaltimes_ci <- quantile(mc_sds_interarrivaltimes, probs = c(0.025, 0.975))
cat('95% CI for Monte Carlo means (inter-arrival times):', mean_mc_interarrivaltimes_ci, '\n')
cat('95% CI for Monte Carlo standard deviations (inter-arrival times):', sd_mc_interarrivaltimes_ci, '\n')

#Histogram for simulated means
hist(mc_means_interarrivaltimes, probability = TRUE, col = 'cornflowerblue',
     main = 'Simulated means versus observed means of inter-arrival times for type II patients',
     xlab = 'Simulated means')
abline(v = mean_arrival_times2, col = 'saddlebrown', lwd = 4)


#Histogram for simulated standard deviations
hist(mc_sds_interarrivaltimes, probability = TRUE, col = 'seagreen3',
     main = 'Simulated standard deviations versus observed standard deviation of inter-arrival times for type II patients',
     xlab = 'Simulated standard deviations')
abline(v = sd_arrival_times2, col = 'brown', lwd = 4)


#Results of initial analysis (mean, sd, median, max, min)
#TYPE1
#Scan duration: mean = 0.4326608 - sd = 0.09777424 - min = 0.09373 - max = 0.70892 - median = 0.43589
#Arrival times/patients per day: mean = 16.47826 - sd = 3.800354 - min = 10 - max = 23 - median = 16
#Inter-arrival times: mean = 0.5453439 - sd = 0.5833021 - min = 0 - max  = 3.05 - median = 0.3750
#TYPE2
#Scan duration: mean = 0.6693389 - sd = 0.1872859 - min = 0.2207 - max = 1.1468 - median = 0.6466 
#Arrival times/patients per day: mean = 10.3913 - sd = 1.233588 - min = 9 - max = 13 - median = 10 
#Inter-arrival times: mean = 0.8666387 - sd = 0.310782 - min = 0.0600 - max  = 1.7500 - median = 0.8750
