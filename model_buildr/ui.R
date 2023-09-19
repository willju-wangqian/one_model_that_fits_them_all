

sidebar <- dashboardSidebar(
  width=230,
  
  sidebarMenu(
    # HTML(
    #   paste0(
    #     "<br>",
    #     "<a href='https://github.com/willju-wangqian/ggpaintr' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='img/ggpaintr.png' width = '170'></a>",
    #     "<br>"
    #   )),
    menuItem("Data", tabName = 'show_data', icon = icon("table")),
    menuItem("Analysis", tabName = "analysis", icon = icon("magnifying-glass-chart"))
    # menuItem("Individual", tabName = "individual", icon = icon("image"))
  )
)



body <- dashboardBody(
  
  tabItems(
    
    tabItem(
      tabName = "show_data",
      sidebarLayout(
        sidebarPanel(
          selectInput("userID", "Select a User ID:", choices = unique(dataset$userID), selected = "8_91"),
          selectInput("intensity", "Select Intensity Values:", choices = unique(dataset$intensity), multiple = TRUE),
          selectInput("distance", "Select Distance Values:", choices = unique(dataset$distance), multiple = TRUE),
        ),
        mainPanel(
          DT::DTOutput("filteredTable"),
        )
      )
      # includeMarkdown("www/home.md"),
      # img(src = "img/image.png", height = 500, width = 1000)
    ),
    
    tabItem(
      tabName = "analysis",
      
      tabsetPanel(
        
        tabPanel(
          "Individual",
          fluidRow(box(
            sidebarLayout(
              sidebarPanel(
                p("Fit a logistic model for one individual with fixed intensity and distance value"),
                selectInput("userID_value", "Select a User ID:", choices = unique(dataset$userID), selected = "8_91"),
                selectInput("intensity_value", "Select a value for intensity:", choices = unique(dataset$intensity), selected = unique(dataset$intensity)[floor(length(unique(dataset$intensity))/2) + 1]),
                selectInput("distance_value", "Select a value for distance:", choices = unique(dataset$distance), selected = unique(dataset$distance)[floor(length(unique(dataset$distance))/2) + 1]),
                checkboxInput("check_intercept", "Should intercept be included?", value = FALSE),
                p("formula of the base model"),
                verbatimTextOutput("base_model_formula", placeholder = TRUE),
                actionButton("base_model_draw", "Draw Plot")
              ),
              mainPanel(
                plotOutput("base_model_logit_fit"),
                verbatimTextOutput("base_model_summary")
              )
            ),
            width = 12
          )),
          
        ),
        
        tabPanel(
          "Fixed Effects",
          fluidRow(box( # distance
            sidebarLayout(
              sidebarPanel(
                HTML("In this section, with data of the same individual, we fix the value of intensity and include <b>distance</b> as a covariate to fit logistic models. <br>"),
                # p("With data of the same individual, fit a logistic model with fixed intensity."),
                # HTML("<b>distance</b> is now included in the model."),
                br(),
                selectInput("intensity_value_distance", "Select a value for the fixed intensity:", choices = unique(dataset$intensity), selected = unique(dataset$intensity)[floor(length(unique(dataset$intensity))/2) + 1]),
                checkboxInput("check_distance_log", "Should log-transformation be applied to distance?", value = TRUE),
                actionButton("distance_model_draw", "Show model fitting results"),
                p("ANOVA result comparing factor distance model and numeric distance model"),
                verbatimTextOutput("anova_distance", placeholder = TRUE),
              ),
              mainPanel(
                p("1. We firstly treat distance as a factor so that each value of distance",
                  "has its own logistic curve. This approach allows us to estimate the effect",
                  "of distance at different levels and helps determine if log-transformation is needed."),
                verbatimTextOutput("distance_factor_formula"),
                plotOutput("distance_factor_fit_plot"),
                # plotOutput("distance_factor_effect_plot"),
                p("2. We then include distance as a numeric covariate and compare the numeric model to the factor model"),
                plotOutput("distance_model_logit_fit_plot"),
                p("formula of the model including numeric distance:"),
                verbatimTextOutput("distance_model_formula", placeholder = TRUE),
                verbatimTextOutput("distance_model_summary")
              )
            ),
            width = 12
          )),
          fluidRow(box( # intensity
            sidebarLayout(
              sidebarPanel(
                HTML("In this section, with data of the same individual, we fix the value of distance and include <b>intensity</b> as a covariate to fit logistic models. <br>"),
                br(),
                # p("Fit a logistic model for one individual with fixed distance."),
                # HTML("<b>intensity</b> is now included in the model."),
                selectInput("distance_value_intensity", "Select a value for the fixed distance:", choices = unique(dataset$distance), selected = unique(dataset$distance)[floor(length(unique(dataset$distance))/2) + 1]),
                checkboxInput("check_intensity_log", "Should log-transformation be applied to intensity?", value = TRUE),
                actionButton("intensity_model_draw", "Show model fitting results"),
                p("ANOVA result comparing factor intensity model and numeric intensity model"),
                verbatimTextOutput("anova_intensity", placeholder = TRUE),
              ),
              mainPanel(
                p("1. We firstly treat intensity as a factor so that each value of intensity",
                  "has its own logistic curve. This approach allows us to estimate the effect",
                  "of intensity at different levels and helps determine if log-transformation is needed."),
                verbatimTextOutput("intensity_factor_formula"),
                plotOutput("intensity_factor_fit_plot"),
                # plotOutput("distance_factor_effect_plot"),
                p("2. We then include intensity as a numeric covariate and compare the numeric model to the factor model"),
                plotOutput("intensity_model_logit_fit_plot"),
                p("formula of the model including numeric intensity:"),
                verbatimTextOutput("intensity_model_formula", placeholder = TRUE),
                verbatimTextOutput("intensity_model_summary")
              )
            ),
            width = 12
          )),
          
          fluidRow(box( # both
            sidebarLayout(
              sidebarPanel(
                HTML("In this section, with data of the same individual, we include both <b>distance</b> and <b>intensity</b> as covariates to fit logistic models. <br>"),
                # p("Fit a logistic model for one individual."),
                # HTML("Both <b>distance</b> and <b>intensity</b> can be included into the model."),
                checkboxInput("include_distance", "include distance?", TRUE),
                checkboxInput("include_intensity", "include intensity?", TRUE),
                actionButton("both_model_draw", "Show model fitting results"),
                p("ANOVA result comparing factor model and numeric model"),
                verbatimTextOutput("anova_both", placeholder = TRUE),
              ),
              mainPanel(
                p('We firstly treat both distance and intensity as factors so that each combination of distance and intensity",
                  "has its own logistic curve. Then we calculate and present the model that treats both variables as numerical variables by clicking the button "Show model fitting results" '),
                verbatimTextOutput("both_factor_formula"),
                plotOutput("both_fit_plot"),
                p("formula of the model including numeric variables:"),
                verbatimTextOutput("both_model_formula", placeholder = TRUE),
                verbatimTextOutput("both_model_summary")
              )
            ),
            width = 12
          )),
          
        ),
        
        # Random Effects
        # tabPanel(
        #   "Random Effects",
        #   fluidRow(box(sidebarLayout(
        #     sidebarPanel(
        #       p("some text"),
        #       actionButton("RE_anova_start", "Compare different Random Effects")
        #     ),
        #     mainPanel(
        #       verbatimTextOutput("RE_anova")
        #     )
        #   ), width = 12)),
        #   
        #   fluidRow(box(sidebarLayout(
        #     sidebarPanel(
        #       selectInput("re_type", "Random effects to be included into the model:",
        #                   choices = c(".",
        #                               "no random effects" = "m0",
        #                               "random effects for signal only" = "m1",
        #                               "random effects for bias and signal" = "m2"), 
        #                   selected = '.')
        #     ),
        #     mainPanel(
        #       verbatimTextOutput("RE_model_summary")
        #     )
        #   ), width = 12)),
        tabPanel(
          "Random Effects",
          fluidRow(box(sidebarLayout(
            sidebarPanel(
              p("In this section, we include data of all participants by introducing a random effect for participants into the GLME model.",
                "We can choose to include or not include distance and intensity as fixed effects based on our previous analysis."),
              actionButton("add_RE", "Add random effects")
            ),
            mainPanel(
              h3("Model summary for the GLME model"),
              verbatimTextOutput("RE_model_summary", placeholder = TRUE)
            )
          ), width = 12)),
          
          fluidRow(box(sidebarLayout(
            sidebarPanel(
              p("Note that: the fixed effect of the insignificant variable should be checked under the GLME model when all participants and random effects are included."),
              uiOutput("select_FE"),
              actionButton("update_FE_of_RE", "Update the fixed effects")
            ),
            mainPanel(
              h3("Model summary for the updated model"),
              verbatimTextOutput("RE_updated_summary", placeholder = TRUE),
              h3("ANOVA table for the previous model and the updated model"),
              verbatimTextOutput("RE_model_comparison_anova", placeholder = TRUE)
            )
          ), width = 12)),
          
          # fluidRow(box(sidebarLayout(
          #   sidebarPanel(
          #     selectInput("re_type", "Random effects to be included into the model:",
          #                 choices = c(".",
          #                             "no random effects" = "m0",
          #                             "random effects for signal only" = "m1",
          #                             "random effects for bias and signal" = "m2"), 
          #                 selected = '.')
          #   ),
          #   mainPanel(
          #     verbatimTextOutput("RE_model_summary")
          #   )
          # ), width = 12)),
          
          
        ),
        
        # tabPanel(
        #   "Individual", icon =  icon("palette"),
        #   box(sidebarLayout(
        #     sidebarPanel(
        #       selectInput("facet1", "Select Facet Grid Row:", choices = c(".", "intensity", "distance"), selected = "."),
        #       selectInput("facet2", "Select Facet Grid Column:", choices = c(".", "intensity", "distance"), selected = "."),
        #       selectInput("groupVar1", "Select Grouping Variable:", choices = c(".", "intensity", "distance"), selected = "."),
        #       actionButton("logistic_regression", "Fit Logistic Regressions"),
        #       actionButton("update_button", "Update"),
        #       textInput("formula_rhs", "Right hand side of regression formula", 
        #                 value = "1 + signal"),
        #       # model statistics: # of parameters; deviance; 
        #       
        #     ),
        #     mainPanel(
        #       plotOutput("filteredPlot"),
        #       verbatimTextOutput("model_summary")
        #     )
        #   ), width = 12)
        # ),
        # 
        # tabPanel(
        #   "Effect",
        #   p("placeholder for explanation"),
        #   sidebarLayout(
        #     sidebarPanel(
        #       actionButton("draw_effect", "Draw Effect"),
        #       selectInput("trans1", "Select Transformation for x-axis:", 
        #                   choices = c("log", "log10","asn", "atanh", "boxcox", 
        #                               "date", "exp", 
        #                               "hms", "identity", "log1p", 
        #                               "log2", "logit", "modulus", "probability", 
        #                               "probit", "pseudo_log", 
        #                               "reciprocal", "reverse", "sqrt"), 
        #                   selected = "identity"),
        #       selectInput("trans2", "Select Transformation for y-axis:", 
        #                   choices = c("log", "log10","asn", "atanh", "boxcox", 
        #                               "date", "exp", 
        #                               "hms", "identity", "log1p", 
        #                               "log2", "logit", "modulus", "probability", 
        #                               "probit", "pseudo_log", 
        #                               "reciprocal", "reverse", "sqrt"), 
        #                   selected = "identity"),
        #       checkboxInput("effect_lm", "Fit geom_smooth?")
        #     ),
        #     mainPanel(
        #       plotOutput("effectPlot")
        #     )
        #   )
        # )
        
        # tabPanel(
        #   "Code", icon =  icon("code")
        #   # box(width = 12, verbatimTextOutput('mycode'))
        # )
      )
    )
    
  )
)


ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title = "Model Buildr"),
  sidebar,
  body
)