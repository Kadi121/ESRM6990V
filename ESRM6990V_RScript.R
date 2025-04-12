# (1) %%%%%%%%%%%%%%%%%%%% Cleaning Student_Home Data %%%%%%%%%%%%%%%%%----
{
  ## Loading relvant libraries and datasets
  
  if (!require("pacman")) {
    install.packages("pacman")
    library(pacman)
  } 
  p_load(tidyverse, dplyr, rstatix, psych, knitr, table1, ggcorrplot, corrplot, regclass, kableExtra, broom, relaimpo, haven, QuantPsyc, jtools, ggeffects, marginaleffects, modelbased, parameters, arm)
  
  load("C:/Users/Henretta Tawiah/Desktop/School/PhD/Spring 2025/R/Project/ESRM6990V/G4_HOME_STUDENT.Rdata")
  
  load("C:/Users/Henretta Tawiah/Desktop/School/PhD/Spring 2025/R/Project/ESRM6990V/G4_STD_CONTEXT.Rdata")
  
  
  load("C:/Users/Henretta Tawiah/Desktop/School/PhD/Spring 2025/R/Project/ESRM6990V/G4_TEACHER_STUDENT.Rdata")
  
  
  ## SUbsetting for Country, Gender, Home Socioeconomic Status/IDX, Parents' Occupation, Instructional Clarity in Mathematics Lessons/IDX, Students Sense of School Belonging/IDX, Students Like Learning Mathematics/IDX, Students Confident in Mathematics/IDX, Home Resources for Learning/IDX, Number of Home Study Supports, 1st to 5th Plausible Math Values, Percent Correct,
  Home_Student <- G4_HOME_STUDENT |> 
    dplyr::select(IDSTUD, CTY, ITSEX, ASDAGE, ASBGDML, ASBGICM, ASBGSEC, ASBGSSB, ASBGSB, ASBGSLM, ASBGSCM, ASDG05S, ASDMCORP)
  
  ## Renaming Columns
  Home_Student2 <- Home_Student |> 
    rename(Student_ID = IDSTUD,Country = CTY, Sex = ITSEX, Age = ASDAGE, 
           Disorderly_Behavior = ASBGDML, Instructional_Clarity = ASBGICM, 
           Digital_Self_Efficacy = ASBGSEC, Sense_of_School_Belonging = ASBGSSB, 
           Student_Bullying = ASBGSB, Like_Learning_Math = ASBGSLM, Confident_in_Math 
           = ASBGSCM, Number_of_Home_Study_Supports = ASDG05S, Math_Percent_Correct = 
             ASDMCORP)
  #summary(Home_Student2)
  
  ## Dropping NAs
  Clean_Home_Student2 <- Home_Student2 |> na.omit()
  #unique(Clean_Home_Student2$Country)
  
  ## Recoding Canada as 1 and USA as 2
  #Clean_Home_Student2$Country <- ifelse(Clean_Home_Student2$Country == "CAN", 1, 2) 
  
  ## Converting Sex and Country Columns to factors
  Final_Home_Student <- Clean_Home_Student2 |> mutate(
    across(c(Country, Sex, Number_of_Home_Study_Supports), ~ as_factor(.x)), Sex = droplevels(Sex), Number_of_Home_Study_Supports = droplevels(Number_of_Home_Study_Supports), Country = droplevels(Country))
  #str(Final_Home_Student)
}

# (2) %%%%%%%%%%%%%%%%%%%% Descriptive Statistics for Student_Home Data %%%%%%%%%%%%%%%%%----
{
  ## Table of Sample Descriptive Statistics
  Tbl1 <- table1(~ Math_Percent_Correct + Age + Sex + Disorderly_Behavior + Student_Bullying + 
            Instructional_Clarity + Digital_Self_Efficacy + Sense_of_School_Belonging + 
            Like_Learning_Math + Confident_in_Math + Number_of_Home_Study_Supports | Country, 
            data = Final_Home_Student)
  
  ## Proportion of Sex by Country
  Student_Sex_by_country <- Final_Home_Student |> group_by(Country, Sex) |> 
    summarize(Count = n(), Proportion = (n() / nrow(Final_Home_Student)) * 100)
  
  ## Plot for Count or Proportion of Various Categorical Predictors (stack vs dodge)
  Fig1 <- ggplot(Student_Sex_by_country, aes(x = Country, y = Count, fill = Sex)) + 
    geom_bar(stat = "identity", position = "dodge") +  labs(x = "Country", y = "NUmber of Students", title = "Number of sudents by Country and Sex") + 
    scale_fill_brewer(palette = "Blues", name = "Level") + theme_bw()
  
  ## Visualizing Math Scores
  Fig2 <- ggplot(Final_Home_Student, aes(x = Math_Percent_Correct, fill = interaction(Country, Sex))) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) + 
    labs(title = "Distribution of Math Scores", x = "Math Score (%)", y = "Frequency") +
    facet_grid(rows = vars(Country), cols = vars(Sex)) + 
    scale_fill_brewer(palette = "Blues") +  # Uses a blue gradient
    theme_bw()
  
  ## Visualizing Math Scores by Country
  Fig3 <- ggplot(Final_Home_Student, aes(x = Country, y = Math_Percent_Correct, fill = Country)) +
    geom_boxplot() +
    labs(title = "Math Scores by Country", x = "Country", y = "Math Score (%)") +
    scale_fill_manual(values = c("#9AD1D0", "#2C5985")) + 
    theme_bw()
  
  ## Visualizing Math Scores by Sex
  Fig4 <- ggplot(Final_Home_Student, aes(x = Sex, y = Math_Percent_Correct, fill = Sex)) +
    geom_boxplot() +
    labs(title = "Math Scores by Sex", x = "Sex", y = "Math Score (%)") +
    scale_fill_manual(breaks = Final_Home_Student$Sex,
                      values = c ("#BCE4D8", "#2D5E88"))+ theme_bw()
  
  ## Visualizing Math Scores by COuntry and Sex
  Fig5 <- ggplot(Final_Home_Student, aes(x = Country, y = Math_Percent_Correct, fill = Sex)) +
    geom_boxplot() +
    labs(title = "Math Scores by Country and Sex", x = "Country", y = "Math Score (%)") +
    scale_fill_brewer("blues")+ theme_bw()
  
  ## Visualizing Math Scores by Home Support
  Fig6 <- ggplot(Final_Home_Student, aes(x = Number_of_Home_Study_Supports, y = Math_Percent_Correct, fill = Number_of_Home_Study_Supports)) +
    geom_boxplot() +
    labs(title = "Math Scores by Number of Home Spport", x = "Sex", y = "Math Score (%)") +
    scale_fill_brewer("blues") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ## Subsetting Muneric Variables only
  std_cor_data <- Final_Home_Student |> dplyr::select(Age:Confident_in_Math, Math_Percent_Correct)
  
  ## Correlation Matrix
  std_cor <- round(cor(std_cor_data), 1)
  
  ## Matrix of Correlation P-Values
  p_std_cor <- cor_pmat(std_cor_data)
  
  ## Correlation Plot
  Fig7 <- ggcorrplot( std_cor, type = "upper", outline.col = "black", 
              ggtheme = ggplot2::theme_minimal, colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)
}

# (3) %%%%%%%%%%%%%%%%%%%% Step-wise Regression Model for Student_Home Data %%%%%%%%%%%%%%%%%----
{
  ## Initial Regression Model
  std_reg_initial <- lm(Math_Percent_Correct ~ . - Student_ID, data = Final_Home_Student)
  
  ## Step-wise regression model
  std_stepwise <- step(std_reg_initial, direction = "both", trace = 0)
  tidy_std_stepwise <- tidy(std_stepwise)
  std_stepwise_summary <- glance(std_stepwise)
  Tbl2 <- tidy_std_stepwise |> 
    kable(digits = 3) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  Tbl3<- std_stepwise_summary |> 
    kable(digits = 3) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  #summary(std_stepwise)
  Tbl4 <- VIF(std_stepwise) |> 
    kable(digits = 3) |> kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Standardized coefficients
  coeff_std <- round(coef(standardize(std_stepwise, standardize.y = TRUE, binary.inputs = "center")), 3)
  coeff_std <- data.frame(Estimate = round(coeff_std, 3))
  rownames(coeff_std) <- c("Intercept", "Country_USA", "Sex_Boy", "Age", "Disorderly_Behavior", "Instructional_Clarity", "Digital_Self_Efficacy", "Student_Bullying", "Like_Learning_Math", "Confident_in_Math", "Number_of_Home_Study_Supports_Either", "Number_of_Home_Study_Supports_Both")
  Tbl5 <- kable(coeff_std)|> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Residual plots in a 2*2 grid
  #par(mfrow=c(2, 2))
  #plot(std_stepwise) -  This is plotted in Quarto file
  
}

# (4) %%%%%%%%%%%%%%%%%%%% Difference-in-Difference Analysis for Student_Home Data %%%%%%%%%%%%%%%%%----
{
  ## Estimating the differences in the average predictions for different country and sex combinations.
  std_pred <- estimate_means(std_stepwise, by = c("Sex", "Country"))
  Tbl6 <- std_pred |> 
    kable(digits = 3) |> kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Testing if these differences are significant
  std_contrast <- estimate_contrasts(std_stepwise, contrast = c("Sex", "Country"))
  Tbl7 <- std_contrast |> 
    kable(digits = 3) |> kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Visualizing the differences in the average predictions for different country and sex combinations.
  #plot(std_pred) -  This is plotted in Quarto file 
}

# (5) %%%%%%%%%%%%%%%%%%%% Cleaning Student_Teacher Data %%%%%%%%%%%%%%%%%----
{
  Teacher_Student <- G4_TEACHER_STUDENT |> 
    dplyr::select(IDTEACH, ATBG01, ATBG02, ATBG03, ATBG04, ATBG10A, ATBM07A, ATBGEAS, ATBGSOS, ATBGTJS, ATBGLSN, ATDMMEM, ATDMHW, ASDMCORP)
  
  ## Renaming Columns
  Teacher_Student2 <- Teacher_Student |> 
    rename(Teacher_ID = IDTEACH, Years_Teaching = ATBG01, Sex = ATBG02, Age = ATBG03, 
           Level_of_Formal_Educ = ATBG04, Class_Size = ATBG10A, 
           Homework_Freq = ATBM07A, Tch_Academic_Success = ATBGEAS, 
           Safe_Orderly_Schools = ATBGSOS, Tch_Job_Satis = ATBGTJS, Student_not_Ready 
           = ATBGLSN, Tch_Educ_Math_Major = ATDMMEM, Math_Instruction_Hours = ATDMHW,
           Math_Percent_Correct = ASDMCORP)
  #str(Teacher_Student2)
  
  ## Dropping NAs
  Clean_Teacher_Student2 <- Teacher_Student2 |> na.omit()
  #str(Clean_Teacher_Student2)
  
  ## Converting Sex and Country Columns to factors
  Final_Teacher_Student <- Clean_Teacher_Student2 |> mutate(
    across(c(Sex, Level_of_Formal_Educ, Homework_Freq, Tch_Educ_Math_Major, Age), ~ as_factor(.x)), 
    Sex = droplevels(Sex), Homework_Freq = droplevels(Homework_Freq), 
    Level_of_Formal_Educ = droplevels(Level_of_Formal_Educ), 
    Tch_Educ_Math_Major = droplevels(Tch_Educ_Math_Major), Age = droplevels(Age))
  #str(Final_Teacher_Student)
}

# (6) %%%%%%%%%%%%%%%%%%%% Sample Descriptive Statistics for Student_Teacher Data %%%%%%%%%%%%%%%%%----
{
  ## Sample Descriptive Statistics
  
  table1(~ Math_Percent_Correct + Age + Sex + Disorderly_Behavior + Student_Bullying +  
           Instructional_Clarity + Digital_Self_Efficacy + Sense_of_School_Belonging + 
           Like_Learning_Math + Confident_in_Math + Number_of_Home_Study_Supports | Country, data = Final_Home_Student)
}