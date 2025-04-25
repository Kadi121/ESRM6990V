# (1) %%%%%%%%%%%%%%%%%%%% Cleaning Student_Home Data %%%%%%%%%%%%%%%%%----
{
  ## Loading relvant libraries and datasets
  
  if (!require("pacman")) {
    install.packages("pacman")
    library(pacman)
  } 
  p_load(tidyverse, dplyr, rstatix, psych, knitr, table1, ggcorrplot, corrplot, regclass, 
         kableExtra, broom, relaimpo, haven, QuantPsyc, jtools, ggeffects, marginaleffects, 
         modelbased, parameters, arm, Hmisc, labelled)
  
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
  Tbl1 <- table1(~ Math_Percent_Correct + Age + Sex + Country + Disorderly_Behavior + Student_Bullying + 
            Instructional_Clarity + Digital_Self_Efficacy + Sense_of_School_Belonging + 
            Like_Learning_Math + Confident_in_Math + Number_of_Home_Study_Supports | Country, 
            data = Final_Home_Student)
  
  ## Proportion of Sex by Country
  Student_Sex_by_country <- Final_Home_Student |> group_by(Country, Sex) |> 
    dplyr::summarize(Count = n(), Proportion = (n() / nrow(Final_Home_Student)) * 100)
  
  ## Plot for Number of sudents by Country and Sex
  Fig1 <- ggplot(Student_Sex_by_country, aes(x = Country, y = Count, fill = Sex)) + 
    geom_bar(stat = "identity", position = "dodge") +  labs(x = "Country", y = "NUmber of Students") + 
    scale_fill_brewer(palette = "Blues", name = "Sex") + theme_bw()
  
  ## Visualizing Math Scores
  Fig2 <- ggplot(Final_Home_Student, aes(x = Math_Percent_Correct, fill = interaction(Country, Sex))) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) + 
    labs(x = "Math Score (%)", y = "Frequency") +
    facet_grid(rows = vars(Country), cols = vars(Sex)) + 
    scale_fill_brewer(palette = "Blues") +  # Uses a blue gradient
    theme_bw()
  
  ## Visualizing Math Scores by Country
  Fig3 <- ggplot(Final_Home_Student, aes(x = Country, y = Math_Percent_Correct, fill = Country)) +
    geom_boxplot() +
    labs(x = "Country", y = "Math Score (%)") +
    scale_fill_manual(values = c("#9AD1D0", "#2C5985")) + 
    theme_bw()
  
  ## Visualizing Math Scores by Sex
  Fig4 <- ggplot(Final_Home_Student, aes(x = Sex, y = Math_Percent_Correct, fill = Sex)) +
    geom_boxplot() +
    labs( x = "Sex", y = "Math Score (%)") +
    scale_fill_manual(breaks = Final_Home_Student$Sex,
                      values = c ("#BCE4D8", "#2D5E88"))+ theme_bw()
  
  ## Visualizing Math Scores by COuntry and Sex
  Fig5 <- ggplot(Final_Home_Student, aes(x = Country, y = Math_Percent_Correct, fill = Sex)) +
    geom_boxplot() +
    labs( x = "Country", y = "Math Score (%)") +
    scale_fill_brewer("blues")+ theme_bw()
  
  ## Visualizing Math Scores by Home Support
  Fig6 <- ggplot(Final_Home_Student, aes(x = Number_of_Home_Study_Supports, y = Math_Percent_Correct, fill = Number_of_Home_Study_Supports)) +
    geom_boxplot() +
    labs( x = "Home Support", y = "Math Score (%)") +
    scale_fill_brewer("blues") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ## Subsetting Numeric Variables only
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
    dplyr::rename(Teacher_ID = IDTEACH, Years_Teaching = ATBG01, Sex = ATBG02, Age = ATBG03, 
           Formal_Educ = ATBG04, Class_Size = ATBG10A, Homework_Freq = ATBM07A, 
           Academic_Success = ATBGEAS, Safe_Orderly_Schools = ATBGSOS, 
           Job_Satis = ATBGTJS, Student_not_Ready = ATBGLSN, Math_Major = ATDMMEM, 
           Instruction_Hours = ATDMHW, Math_Percent_Correct = ASDMCORP)
  #str(Teacher_Student2)
  
  ## Dropping NAs
  Clean_Teacher_Student2 <- Teacher_Student2 |> na.omit()
  #str(Clean_Teacher_Student2)
  
  ## Recode labels for Formal_Educ
  Clean_Teacher_Student2$Formal_Educ <- factor(
    Clean_Teacher_Student2$Formal_Educ,
    levels = c(3, 4, 5, 6, 7),
    labels = c("Post-secondary, non-tertiary education",
               "Short-cycle tertiary education",
               "Bachelor’s or equivalent",
               "Master’s or equivalent",
               "Doctor or equivalent")
  )

  ## Converting Sex and Homework_Freq, Math_Major, Age Columns to factors
  Final_Teacher_Student <- Clean_Teacher_Student2 |> mutate(
    across(c(Sex, Homework_Freq, Math_Major, Age), ~ as_factor(.x)), 
    Sex = droplevels(Sex), Homework_Freq = droplevels(Homework_Freq), 
    Formal_Educ = droplevels(Formal_Educ), Math_Major = droplevels(Math_Major), 
    Age = droplevels(Age))
  #str(Final_Teacher_Student)
  
  ## Recode <other> as Other in Sex
  Final_Teacher_Student$Sex <- as.character(Final_Teacher_Student$Sex)
  Final_Teacher_Student$Sex[Final_Teacher_Student$Sex == "<Other>"] <- "Other"
  Final_Teacher_Student$Sex <- factor(Final_Teacher_Student$Sex)
  
  # Convert all labelled variables:
  Final_Teacher_Student <- Final_Teacher_Student |> 
    mutate(across(where(is.labelled), ~ if (is.numeric(.)) as.numeric(.) else to_factor(.)))
}

# (6) %%%%%%%%%%%%%%%%%%%% Sample Descriptive Statistics for Student_Teacher Data %%%%%%%%%%%%%%%%%----
{
  # Assigning descriptive labels to variables
  label(Final_Teacher_Student$Math_Percent_Correct) <- "Mathematics Percent Correct Points Scored"
  label(Final_Teacher_Student$Age) <- "Age of Teacher"
  label(Final_Teacher_Student$Sex) <- "Sex of Teacher"
  label(Final_Teacher_Student$Years_Teaching) <- "Years of Teaching"
  label(Final_Teacher_Student$Class_Size) <- "Number of Students in Class"
  label(Final_Teacher_Student$Homework_Freq) <- "Homework Frequency"
  label(Final_Teacher_Student$Formal_Educ) <- "Level of Formal Education Completed"
  label(Final_Teacher_Student$Academic_Success) <- "School Emphasis on Teacher's Academic Success"
  label(Final_Teacher_Student$Safe_Orderly_Schools) <- "Safe and Orderly Schools-Teacher"
  label(Final_Teacher_Student$Job_Satis) <- "Teachers Job Satisfaction"
  label(Final_Teacher_Student$Student_not_Ready) <- "Teaching Limited by Student Not Ready"
  label(Final_Teacher_Student$Math_Major) <- "Teachers Majored in Education and Mathematics"
  
  ## Sample Descriptive Statistics
  tch_tb1 <- table1(~ Math_Percent_Correct + Age + Sex  + Formal_Educ + Years_Teaching + Class_Size + 
                      Homework_Freq + Academic_Success + Safe_Orderly_Schools + Job_Satis + Student_not_Ready +
                      Math_Major| Sex, data = Final_Teacher_Student)
  
  ## Proportion of Age and Formal Education
  Age_by_Form_Educ <- Final_Teacher_Student |> group_by(Age, Formal_Educ) |> 
    dplyr::summarise(Count = n(), Proportion = (n() / nrow(Final_Teacher_Student)) * 100)
  
  ## Plot of Number of Teachers by Age and Education Level
  tch_fig1 <- ggplot(Age_by_Form_Educ, aes(x = Age, y = Count, fill = Formal_Educ)) + 
    geom_bar(stat = "identity", position = "dodge") +  
    labs(x = "Age", y = "NUmber of Teachers") + 
    scale_fill_brewer(palette = "Blues", name = "Level of Formal Education") + theme_bw()
  
  ## Visualizing Math Scores by Age and Education Level
  tch_fig2 <- ggplot(Final_Teacher_Student, aes(x = Age, y = Math_Percent_Correct, fill = Formal_Educ)) +
    geom_boxplot() +
    labs( x = "Age", y = "Math Score (%)") +
    scale_fill_brewer("blues")+ theme_bw()
  
  ## Visualizing Math Scores by Teacher's Age
  tch_fig3 <- ggplot(Final_Teacher_Student, aes(x = Age, y = Math_Percent_Correct, fill = Age)) +
    geom_boxplot() +
    labs( x = "Age", y = "Math Score (%)") +
    scale_fill_manual(values = c("#9AD1D0", "#2C5985","#5E9FA3", "#B5CFEA", "#4C7C9B", "#A3D5D3")) + 
    theme_bw()
  
  ## Visualizing Math Scores by Teacher's Level of Formal Education
  tch_fig4 <- ggplot(Final_Teacher_Student, aes(x = Formal_Educ, y = Math_Percent_Correct, fill = Formal_Educ)) +
    geom_boxplot() +
    labs( x = "Formal Education", y = "Math Score (%)") +
    scale_fill_manual(values = c("#BCE4D8", "#2D5E88","#83B8B0", "#A1C4E4", "#3F8DAE")) + 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ## Visualizing Math Scores by Sex
  tch_fig5 <- ggplot(Final_Teacher_Student, aes(x = Sex, y = Math_Percent_Correct, fill = Sex)) +
    geom_boxplot() +
    labs(x = "Sex", y = "Math Score (%)") +
    scale_fill_brewer("blues") +
    theme_bw()
  
  ## Sub-setting Numeric Variables only
  tch_cor_data <- Final_Teacher_Student |> dplyr::select(Math_Percent_Correct, Years_Teaching, Class_Size, 
                                                         Academic_Success:Student_not_Ready, Instruction_Hours)
  
  ## Correlation Matrix
  tch_cor <- round(cor(tch_cor_data), 1)
  
  ## Matrix of Correlation P-Values
  p_tch_cor <- cor_pmat(tch_cor_data)
  
  ## Correlation Plot
  tch_fig6 <- ggcorrplot( tch_cor, type = "upper", outline.col = "black", 
                      ggtheme = ggplot2::theme_minimal, colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)
}

# (7) %%%%%%%%%%%%%%%%%%%% Step-wise Regression Analysis for Student_Teacher Data %%%%%%%%%%%%%%%%%----
{
  ## Initial Regression Model
  tch_reg_initial <- lm(Math_Percent_Correct ~ . - Teacher_ID, data = Final_Teacher_Student)
  
  ## Step-wise regression model
  tch_stepwise <- step(tch_reg_initial, direction = "both", trace = 0)
  tidy_tch_stepwise <- tidy(tch_stepwise)
  tch_stepwise_summary <- glance(tch_stepwise)
  tch_tb2 <- tidy_tch_stepwise |> 
    kable(digits = 3) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  tch_tb3<- tch_stepwise_summary |> 
    kable(digits = 3) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  #summary(std_stepwise)
  tch_tb4 <- VIF(tch_stepwise) |> 
    kable(digits = 3) |> kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Standardized coefficients
  coeff_tch <- round(coef(standardize(tch_stepwise, standardize.y = TRUE, binary.inputs = "center")), 3)
  coeff_tch <- data.frame(Estimate = round(coeff_tch, 3))
  rownames(coeff_tch) <- c("Intercept", "Years_Teaching", "Sex_Male", "Sex_Other", "Age(25–29)", 
                           "Age (30–39)", "Age (40–49)", "Age (50–59)", "Age (60 or more)", 
                           "Formal_Educ (Short-cycle tertiary education)", "Formal_Educ (Bachelor’s or equivalent)", 
                           "Formal_Educ (Master’s or equivalent)", "Formal_Educ (Doctor or equivalent)", "Class_Size", 
                           "Homework_Freq (Less than once a week)", "Homework_Freq (1 or 2 times a week)", 
                           "Homework_Freq (3 or 4 times a week)", "Homework_Freq (Every day)", "Academic_Success", 
                           "Safe_Orderly_Schools", "Job_Satis", "Student_not_Ready", "Math_Major (Major in Edu but not Math)", 
                           "Math_Major (Major in Math but not Edu)", "Math_Major(All other Majors)", "Instruction_Hours")

  tch_tb5 <- kable(coeff_tch)|> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Residual plots in a 2*2 grid
  #par(mfrow=c(2, 2))
  #plot(tch_stepwise) -  This is plotted in Quarto file 
}

# (8) %%%%%%%%%%%%%%%%%%%% Marginal Mean/ Contrast Analysis for Student_Teacher Data %%%%%%%%%%%%%%%%%----
{
  ## Age-Level Analysis
  age_pred <- estimate_means(tch_stepwise, "Age")
  tch_fig7 <- ggplot(age_pred, aes(x = Age, y = Mean, group = 1)) +
    geom_line(color = "black") +
    geom_point(size = 2, color = "black") +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
    labs(
      x = "Age of Teacher",
      y = "Mean of Math Percent Scored") +
    theme_bw()
  
  ## Marginal Contrasts for Age-Level Analysis
  age_cont <- estimate_contrasts(tch_stepwise, "Age", p_adjust = "bonferroni")
  tch_tb6<- age_cont |> 
    kable(digits = 3) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  ## Education-Level Analysis
  educ_pred <- estimate_means(tch_stepwise, "Formal_Educ")
  tch_fig8 <- ggplot(educ_pred, aes(x = Formal_Educ, y = Mean, group = 1)) +
    geom_line(color = "black") +
    geom_point(size = 2, color = "black") +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
    labs(
      x = "Teacher's Level of Formal Education",
      y = "Mean of Math Percent Scored") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ## Marginal Contrasts for Education-Level Analysis
  educ_cont <- estimate_contrasts(tch_stepwise, "Formal_Educ", p_adjust = "bonferroni")
  tch_tb7<- educ_cont |> 
    kable(digits = 3) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}