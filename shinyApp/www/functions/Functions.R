

#####Function

histogram <- function(df,year,country,causes,sex,age_group){
  
  filtered_df <- filter(df, Year %in% year , Country %in% country, Cause %in% causes, Sex %in% sex, Age_group %in% age_group )
  
  aggregated_df <- aggregate(cbind(mortality) ~ Country + Year + Sex +Age_group, data = filtered_df, FUN = sum)
  
  # Create the histogram using ggplot2
  my_plot<-
    ggplot(aggregated_df, aes(x=Age_group, y=mortality,fill = Sex))+
    geom_bar(stat = "identity", position = "dodge")+
    xlab("Age distribution") + ylab("number of Mortality") + ggtitle(paste("Mortality by Age Group,",country))+
    scale_x_discrete(labels = c("Age00_01" = "0-1", "Age01_04" = "1-4", "Age05_14" = "5-14", "Age15_24" = "15-24", "Age25_34" = "25-34", "Age35_44" = "35-44", "Age45_54" = "45-54", "Age55_64" = "55-64", "Age65above" = "65+", "Unspecified_Age" = "Unknown"))+
    theme_bw()+
    theme(plot.title = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 45))
  
  
  ggplotly(my_plot, tooltip = c("y"))
}

histogram_courses_distribution <- function(df,year,country,sex,age_group,causes,Sort){
  
  filtered_df <- filter(df, Year %in% year, Sex %in% sex, Country %in% country, Cause %in% causes, Age_group %in% age_group)
  filtered_df <- aggregate(cbind(mortality) ~ Country + Year+Sex+Cause, data = filtered_df, FUN = sum)
  if (Sort == "From Low to High") {
    filtered_df$Cause <- factor(filtered_df$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(Mortality) %>% pull(Cause))
  } else if (Sort == "From High to Low") {
    filtered_df$Cause <- factor(filtered_df$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(desc(Mortality)) %>% pull(Cause))
  } else {
    stop("Invalid value for Sort parameter. Use 'From Low to High' or 'From High to Low'.")
  }
  
  # Create the histogram using ggplot2
  my_plot <-
    ggplot(filtered_df , aes(x = Cause, y = mortality,fill=mortality)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Causes") +
    ylab("number of Mortality") + ggtitle("Mortality Numbers by causes") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())  
  
  ggplotly(my_plot, tooltip = c("x", "y"))
  
  
}


tab3_bar2 <- function(df,year,country,sex,age_group,causes,Sort){
  filtered_df <- filter(df, Year %in% year, Sex %in% sex, Country %in% country, Cause %in% causes, Age_group %in% age_group)
  if (Sort == "From Low to High") {
    filtered_df$Cause <- factor(filtered_df$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(Mortality) %>% pull(Cause))
  } else if (Sort == "From High to Low") {
    filtered_df$Cause <- factor(filtered_df$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(desc(Mortality)) %>% pull(Cause))
  } else {
    stop("Invalid value for Sort parameter. Use 'From Low to High' or 'From High to Low'.")
  }
  ggplot(filtered_df) +
    geom_col(aes(x = mortality, y = Cause, fill = Age_group), position = "fill", alpha = 0.5) + 
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  
}

tab3_bar1 <- function(df,year,country,sex,age_group,causes,Sort){
  filtered_df <- filter(df, Year %in% year, Sex %in% sex, Country %in% country, Cause %in% causes, Age_group %in% age_group)
  aggregated_all_age <- aggregate(cbind(mortality) ~ Country + Year+Sex+Cause, data = filtered_df, FUN = sum)
  if (Sort == "From Low to High") {
    aggregated_all_age$Cause <- factor(aggregated_all_age$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(Mortality) %>% pull(Cause))
  } else if (Sort == "From High to Low") {
    aggregated_all_age$Cause <- factor(aggregated_all_age$Cause, levels = filtered_df %>% group_by(Cause) %>% summarise(Mortality = sum(mortality)) %>% arrange(desc(Mortality)) %>% pull(Cause))
  } else {
    stop("Invalid value for Sort parameter. Use 'From Low to High' or 'From High to Low'.")
  }
  # Create the histogram using ggplot2
  threshold <- max(aggregated_all_age$mortality)/2
  ggplot(aggregated_all_age , aes(x = mortality, y = Cause,fill=mortality)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = mortality,hjust = ifelse(mortality > threshold,-0.5, 1.2)), vjust = 0.7,fontface = "bold") +
    labs(x=NULL,y=NULL) +
    scale_x_reverse() +
    coord_cartesian(clip = 'off') +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 12, face = "bold")) +
    scale_y_discrete(position = "right")+
    scale_fill_gradient(low = "#FFCCCC", high = "#CCCCFF")
  
}


plot4 <- function(data, year, country, sex,causes) {
  
  data <- filter(data, Year %in% year, Country %in% country, Sex %in% sex, Cause %in% causes)
  
  summarized_df <- data %>%
    group_by(Cause) %>%
    summarise_at(vars('Age00_01', 'Age01_04', 'Age05_14', 'Age15_24', 'Age25_34', 'Age35_44', 'Age45_54', 'Age55_64', 'Age65above', 'Unspecified_Age'), sum) %>%
    mutate_at(vars(-Cause), rescale)
  
  summarized_df <- data.frame(Cause = summarized_df$Cause, summarized_df[, -1])
  rownames(summarized_df) <- summarized_df$Cause
  summarized_df <- summarized_df[, -1]
  max_val <- max(summarized_df)
  summarized_df <- rbind(rep(max_val, times = ncol(summarized_df)), summarized_df)
  
  cause_colors <- colorRampPalette(brewer.pal(9, "Set1"))(nrow(summarized_df) - 1)
  cause_colors_alpha <- sapply(cause_colors, function(x) adjustcolor(x, alpha.f = 0.5), simplify = FALSE)
  
  colnames(summarized_df) <- c('Age00_01', 'Age01_04', 'Age05_14', 'Age15_24', 'Age25_34', 'Age35_44', 'Age45_54', 'Age55_64', 'Age65above', 'Unspecified_Age')
  
  # Increase the cex value to make the radar chart elements larger
  radarchart(summarized_df, pcol = cause_colors, plwd = 2, plty = 1, pfcol = unlist(cause_colors_alpha), cglcol = 'grey', cglty = 1, axislabcol = 'grey', caxislabels = seq(0, max_val, by = 0.1), cglwd = 0.8, cex = 2)
  
  legend("topright", inset = c(-0.15, 0), legend = rownames(summarized_df)[-1], col = cause_colors, bty = "n", cex = 1, pt.cex = 1.5, pch = 20, lty = 1, horiz = FALSE, ncol = 1)
}

line3_A <-function(df,year,country,causes,type){
  if(type == "Age"){
    
    filtered_df <- filter(df, Year %in% year , Country %in% country,Cause %in% causes)
    
    aggregated_df <- aggregate(cbind(mortality) ~ Country + Year + Age_group, data = filtered_df, FUN = sum)
    
    forecast_result <- forecast_age_group(aggregated_df)
    future_data <- forecast_result$future_data
    future_years <- forecast_result$future_years
    df_all <- bind_rows(aggregated_df, future_data)  
    
    my_plot <- ggplot(df_all, aes(x = Year, y = mortality, color = Age_group)) +
      geom_line(aes(linetype = if_else(Year <= max(aggregated_df$Year) & Year != min(future_years$Year), "Observed", "Predicted"))) +
      geom_line(data = filter(df_all, Year == max(aggregated_df$Year) | Year == min(future_years$Year)), aes(linetype = "Dashed Connection"), size = 1) +
      geom_point() +
      theme(axis.line = element_line(colour = "black", size = 1),
            panel.grid.major.y = element_line(color = "#EAEAEA",size = 2),
            panel.grid.major.x= element_blank(),
            panel.background = element_blank()) +
      scale_x_continuous(breaks = df_all$Year, labels = df_all$Year) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed", "Dashed Connection" = "dashed"))
    labs(linetype = "")
    
    my_plot <-ggplotly(my_plot, tooltip = c("x", "y"))
    my_plot <- my_plot %>% layout(legend = list(title = list(text = "")))
    my_plot
    
  }else{
    filtered_df <- filter(df, Year %in% year , Country %in% country,Cause %in% causes)
    
    aggregated_df <- aggregate(cbind(mortality) ~ Country + Year + Sex, data = filtered_df, FUN = sum)
    
    forecast_result <- forecast_sex(aggregated_df)
    future_data <- forecast_result$future_data
    future_years <- forecast_result$future_years
    df_all <- bind_rows(aggregated_df, future_data)
    
    
    my_plot <- ggplot(df_all, aes(x = Year, y = mortality, color = Sex)) +
      geom_line(aes(linetype = if_else(Year <= max(aggregated_df$Year) & Year != min(future_years$Year), "Observed", "Predicted"))) +
      geom_line(data = filter(df_all, Year == max(aggregated_df$Year) | Year == min(future_years$Year)), aes(linetype = "Dashed Connection"), size = 1) +
      geom_point() +
      theme(axis.line = element_line(colour = "black", size = 1),
            panel.grid.major.y = element_line(color = "#EAEAEA",size = 2),
            panel.grid.major.x= element_blank(),
            panel.background = element_blank()) +
      scale_x_continuous(breaks = df_all$Year, labels = df_all$Year) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed", "Dashed Connection" = "dashed"))
    labs(linetype = "")
    
    my_plot <-ggplotly(my_plot, tooltip = c("x", "y"))
    my_plot <- my_plot %>% layout(legend = list(title = list(text = "")))
    my_plot
    
  }
}

forecast_sex <- function(aggregated_df) {
  future_years <- data.frame(Year = (max(aggregated_df$Year) + 1):(max(aggregated_df$Year) + 5))
  
  sex_levels <- unique(aggregated_df$Sex)
  
  future_data <- lapply(sex_levels, function(sex) {
    model <- lm(mortality ~ Year, data = filter(aggregated_df, Sex == sex))
    mutate(future_years, Sex = sex, mortality = predict(model, future_years))
  })
  
  list(future_data = bind_rows(future_data), future_years = future_years)
}

forecast_age_group <- function(aggregated_df) {
  future_years <- data.frame(Year = (max(aggregated_df$Year) + 1):(max(aggregated_df$Year) + 5))
  
  Age_group_levels <- unique(aggregated_df$Age_group)
  
  future_data <- lapply(Age_group_levels, function(age_group) {
    model <- lm(mortality ~ Year, data = filter(aggregated_df, Age_group == age_group))
    mutate(future_years, Age_group = age_group, mortality = predict(model, future_years))
  })
  
  list(future_data = bind_rows(future_data), future_years = future_years)
}

line3_B <- function(df,year,country, sex, age_group,causes){
  
  filtered_df <- filter(df, Year %in% year , Country %in% country, Sex %in% sex ,Cause %in% causes,Age_group %in% age_group)
  
  aggregated_df <- aggregate(cbind(mortality) ~ Country + Year + Sex, data = filtered_df, FUN = sum)
  
  
  forecast_result <- forecast_country(aggregated_df)
  future_data <- forecast_result$future_data
  future_years <- forecast_result$future_years
  df_all <- bind_rows(aggregated_df, future_data)
  
  
  my_plot <- ggplot(df_all, aes(x = Year, y = mortality, color = Country)) +
    geom_line(aes(linetype = if_else(Year <= max(aggregated_df$Year) & Year != min(future_years$Year), "Observed", "Predicted"))) +
    geom_line(data = filter(df_all, Year == max(aggregated_df$Year) | Year == min(future_years$Year)), aes(linetype = "Dashed Connection"), size = 1) +
    geom_point() +
    theme(axis.line = element_line(colour = "black", size = 1),
          panel.grid.major.y = element_line(color = "#EAEAEA",size = 2),
          panel.grid.major.x= element_blank(),
          panel.background = element_blank()) +
    scale_x_continuous(breaks = df_all$Year, labels = df_all$Year) +
    scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed", "Dashed Connection" = "dashed"))
  labs(linetype = "")
  
  my_plot <-ggplotly(my_plot, tooltip = c("x", "y"))
  my_plot <- my_plot %>% layout(legend = list(title = list(text = "")))
  my_plot
  
}

forecast_country <- function(aggregated_df) {
  future_years <- data.frame(Year = (max(aggregated_df$Year) + 1):(max(aggregated_df$Year) + 5))
  
  country_levels <- unique(aggregated_df$Country)
  
  future_data <- lapply(country_levels, function(country) {
    model <- lm(mortality ~ Year, data = filter(aggregated_df, Country == country))
    mutate(future_years, Country = country, mortality = predict(model, future_years))
  })
  
  list(future_data = bind_rows(future_data), future_years = future_years)
}

vb <- function(df,country, year ){
  
  filtered_df <- filter(df, Country %in% country, Year %in% year, Sex =="All")
  
  aggregated_df <- aggregate(cbind(mortality) ~ Country + Year, data = filtered_df, FUN = sum)
  
  aggregated_df$mortality[1]
  
  
}

