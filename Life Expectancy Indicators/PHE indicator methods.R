library(dplyr)
library(PHEindicatormethods)

data <- data.frame(area = c(rep("Area1", 10), rep("Area2", 10)),
                   decile = c(1:10, 1:10),
                   population = c(7291, 7997, 6105, 7666, 5790, 6934, 5918, 5974, 7147, 7534, 21675,
                                  20065, 19750, 24713, 20112, 19618, 22408, 19752, 18939, 19312),
                   value = c(75.9, 78.3, 83.8, 83.6, 80.5, 81.1, 81.7, 84.2, 80.6, 86.3, 70.5,
                             71.6, 72.5, 73.5, 73.1, 76.2, 78.7, 80.6, 80.9, 80),
                   lowerCL = c(72.7,75.3,80.9,80.2,77.1,78,79,81.4,75.8,83.2,
                               70.1,71.1,72,73.1, 72.7, 75.7, 78.2,80.1,80.4,79.5),
                   upperCL = c(79.1,81.4,86.8,87.1,83.8,84.2,84.4,86.9,85.4,
                               89.4,71,72.1,73.2,73.7,75.8,78.8,79.8,81.2,81.3,80.9),
                   StandardError = c(1.64,1.58,1.51,1.78,1.7,1.56,1.37,1.4,2.43,
                                     1.57,0.23,0.26,0.3,0.16,0.79,0.78,0.4,0.28,0.23,0.35)
)


# Run SII function on the two areas in the data
phe_sii(group_by(data, area),
        decile,
        population,
        value_type = 0, # default normal distribution
        value = value,
        lower_cl = lowerCL,
        upper_cl = upperCL,
        confidence = 0.95,
        rii = TRUE,
        type = "standard")

# Supplying the standard error instead of the indicator 95 percent confidence limits
# gives the same result
phe_sii(group_by(data, area),
        decile,
        population,
        value_type = 0,
        value = value,
        se = StandardError,
        confidence = 0.95,
        rii = TRUE,
        type = "standard")

# multiple confidence intervals, log transforming the data if they are rates
phe_sii(group_by(data, area),
        decile,
        population,
        value_type = 1,
        transform = TRUE,
        value = value,
        lower_cl = lowerCL,
        upper_cl = upperCL,
        confidence = c(0.95, 0.998),
        repetitions = 10000,
        rii = TRUE,
        type = "standard")
