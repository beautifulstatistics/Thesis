# 1_explore_features.R
# Initial exploration of the counts.db database structure and statistics
# This script explores the tables, their schemas, and basic statistics

# Load necessary libraries and helper functions
source("src/utils/helper_functions.R")

# Connect to the database
connectdB()

# Get list of all tables in the database
cat("Tables in the database:\n")
tables <- dbListTables(conn)
print(tables)
cat("\n")

# Function to get table structure
get_table_structure <- function(table_name) {
  query <- paste0("PRAGMA table_info(", table_name, ")")
  structure <- dbGetQuery(conn, query)
  return(structure)
}

# Function to get table row count
get_table_count <- function(table_name) {
  query <- paste0("SELECT COUNT(*) as count FROM ", table_name)
  count <- dbGetQuery(conn, query)
  return(count$count)
}

# Explore each table
for (table in tables) {
  cat("=============================================\n")
  cat("Table:", table, "\n")
  
  # Get structure
  cat("Structure:\n")
  structure <- get_table_structure(table)
  print(structure)
  
  # Get row count
  count <- get_table_count(table)
  cat("\nRow count:", format(count, big.mark=","), "\n")
  
  # Get first few rows
  cat("\nSample data (first 5 rows):\n")
  sample_data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " LIMIT 5"))
  print(sample_data)
  
  cat("\n")
}

# Explore the censorship distribution across tables
explore_censorship <- function() {
  cat("=============================================\n")
  cat("Censorship Distribution Analysis:\n\n")
  
  # Check presence table censorship
  if ("presence" %in% tables) {
    cat("Censorship distribution in 'presence' table:\n")
    censorship_query <- "SELECT permission_denied, COUNT(*) as count FROM presence GROUP BY permission_denied"
    censorship_stats <- dbGetQuery(conn, censorship_query)
    print(censorship_stats)
    
    # Calculate percentage
    total <- sum(censorship_stats$count)
    censorship_stats$percentage <- (censorship_stats$count / total) * 100
    cat("Percentage of censored posts:", 
        format(censorship_stats$percentage[censorship_stats$permission_denied == 1], digits=4), "%\n\n")
  }
  
  # Check aggregated_binomial table
  if ("aggregated_binomial" %in% tables) {
    cat("Censorship summary in 'aggregated_binomial' table:\n")
    agg_query <- "SELECT 
                   SUM(POSITIVE) as total_censored,
                   SUM(TOTAL) as total_posts,
                   (SUM(POSITIVE) * 100.0 / SUM(TOTAL)) as censorship_rate
                 FROM aggregated_binomial"
    agg_stats <- dbGetQuery(conn, agg_query)
    print(agg_stats)
    cat("\n")
    
    # Distribution of censorship rates
    cat("Distribution of censorship rates in aggregated_binomial:\n")
    rate_query <- "SELECT 
                    CASE 
                      WHEN POSITIVE = 0 THEN '0%'
                      WHEN POSITIVE = TOTAL THEN '100%'
                      WHEN (POSITIVE * 100.0 / TOTAL) < 25 THEN '1-25%'
                      WHEN (POSITIVE * 100.0 / TOTAL) < 50 THEN '25-50%'
                      WHEN (POSITIVE * 100.0 / TOTAL) < 75 THEN '50-75%'
                      ELSE '75-99%'
                    END as censorship_rate,
                    COUNT(*) as count
                  FROM aggregated_binomial
                  GROUP BY censorship_rate
                  ORDER BY 
                    CASE censorship_rate
                      WHEN '0%' THEN 0
                      WHEN '1-25%' THEN 1
                      WHEN '25-50%' THEN 2
                      WHEN '50-75%' THEN 3
                      WHEN '75-99%' THEN 4
                      WHEN '100%' THEN 5
                    END"
    rate_stats <- dbGetQuery(conn, rate_query)
    print(rate_stats)
    cat("\n")
  }
  
  # Check aggregated_binomial_positive table
  if ("aggregated_binomial_positive" %in% tables) {
    cat("Censorship analysis in 'aggregated_binomial_positive' table:\n")
    pos_query <- "SELECT 
                   COUNT(*) as total_rows,
                   SUM(CASE WHEN POSITIVE = TOTAL THEN 1 ELSE 0 END) as all_censored,
                   (SUM(CASE WHEN POSITIVE = TOTAL THEN 1 ELSE 0 END) * 100.0 / COUNT(*)) as percent_all_censored
                 FROM aggregated_binomial_positive"
    pos_stats <- dbGetQuery(conn, pos_query)
    print(pos_stats)
    cat("\n")
    
    # Get statistics on POSITIVE/TOTAL ratio
    ratio_query <- "SELECT 
                     MIN(POSITIVE * 1.0 / TOTAL) as min_ratio,
                     MAX(POSITIVE * 1.0 / TOTAL) as max_ratio,
                     AVG(POSITIVE * 1.0 / TOTAL) as avg_ratio,
                     MEDIAN(POSITIVE * 1.0 / TOTAL) as median_ratio
                   FROM aggregated_binomial_positive"
    ratio_stats <- dbGetQuery(conn, ratio_query)
    print(ratio_stats)
    cat("\n")
  }
}

# Run the censorship exploration
explore_censorship()

# Look at linguistic category distributions in the tables
explore_linguistic_categories <- function() {
  cat("=============================================\n")
  cat("Linguistic Category Analysis:\n\n")
  
  # Get categories from the 'presence' table (if it exists)
  if ("presence" %in% tables) {
    # Get list of all fields except ID and permission_denied
    fields_query <- "PRAGMA table_info(presence)"
    fields <- dbGetQuery(conn, fields_query)
    category_fields <- fields$name[!fields$name %in% c("ID", "permission_denied")]
    
    cat("Found", length(category_fields), "linguistic categories in the database\n")
    
    # Sample some categories to show their prevalence
    if (length(category_fields) > 0) {
      sample_size <- min(10, length(category_fields))
      sample_categories <- sample(category_fields, sample_size)
      
      cat("\nPrevalence of sample linguistic categories:\n")
      for (category in sample_categories) {
        query <- paste0("SELECT SUM(", category, ") as sum, AVG(", category, 
                        ")*100 as percentage FROM presence")
        stats <- dbGetQuery(conn, query)
        cat(category, ": Present in", 
            format(stats$percentage, digits=4, nsmall=2), "% of posts\n")
      }
    }
    
    # Look at the combined presence of top-level categories
    cat("\nPrevalence of top-level categories (from models.R):\n")
    source("src/utils/models.R")
    for (category in nesting_overall$top) {
      if (category %in% category_fields) {
        query <- paste0("SELECT SUM(", category, ") as sum, AVG(", category, 
                        ")*100 as percentage FROM presence")
        stats <- dbGetQuery(conn, query)
        cat(category, ": Present in", 
            format(stats$percentage, digits=4, nsmall=2), "% of posts\n")
      }
    }
  }
}

# Run the linguistic category exploration
explore_linguistic_categories()

# Investigate the relationship between censorship and linguistic categories
explore_censorship_relationships <- function() {
  cat("=============================================\n")
  cat("Preliminary Censorship-Category Relationships:\n\n")
  
  # Check if 'presence' table exists
  if ("presence" %in% tables) {
    # Get categories from the table
    fields_query <- "PRAGMA table_info(presence)"
    fields <- dbGetQuery(conn, fields_query)
    category_fields <- fields$name[!fields$name %in% c("ID", "permission_denied")]
    
    # Look at top-level categories
    top_categories <- intersect(nesting_overall$top, category_fields)
    
    if (length(top_categories) > 0) {
      cat("Relationship between top-level categories and censorship:\n\n")
      
      for (category in top_categories) {
        query <- paste0("SELECT ", category, ", 
                         SUM(permission_denied) as censored,
                         COUNT(*) as total,
                         (SUM(permission_denied) * 100.0 / COUNT(*)) as censorship_rate
                       FROM presence
                       GROUP BY ", category, 
                       " ORDER BY ", category)
        stats <- dbGetQuery(conn, query)
        
        cat("Category:", category, "\n")
        print(stats)
        
        # Calculate significance using chi-square test
        contingency <- matrix(c(stats$censored[stats[[category]] == 1], 
                               stats$total[stats[[category]] == 1] - stats$censored[stats[[category]] == 1],
                               stats$censored[stats[[category]] == 0], 
                               stats$total[stats[[category]] == 0] - stats$censored[stats[[category]] == 0]),
                             nrow = 2)
        chi_test <- chisq.test(contingency)
        cat("Chi-square test: X-squared =", chi_test$statistic, 
            ", p-value =", format(chi_test$p.value, digits=4), "\n\n")
      }
    }
  }
}

# Run the censorship-relationship exploration
explore_censorship_relationships()

# Investigate the nested structure visualization
explore_nested_structure <- function() {
  cat("=============================================\n")
  cat("Nested Structure Visualization:\n\n")
  
  # Generate a simple visualization of the nesting structure
  source("src/utils/models.R")
  
  # Display top-level to bottom-level relationships
  cat("Top-level to bottom-level nesting relationships:\n")
  for (category in names(nesting_bottom)) {
    cat(category, "contains:", 
        paste(setdiff(nesting_bottom[[category]]$bottom, c("tokencount", "image")), collapse=", "), 
        "\n")
  }
}

# Run the nested structure exploration
explore_nested_structure()

disconnectdB()