

function(input, output) {
    
    output$introtext <- renderText(
        "A real estate company in NYC is looking for properties to invest in. They spend a lot of time \nsearching for profitable properties. They need help streamlining this process and making it more efficient. \n
         
        I will find properties for them to invest in by: \n
        - looking into a dataset of past NYC property sales\n
        - finding out what makes a property profitable \n
        - finding out what makes a profitable property different than other properties"
    )
    
    output$cleaningtext <- renderText(
        "--- Define Profitable Property ---
I defined a 'profitable property' by:
- a property that sold for lower than the average sale price of properties in the same
neighborhood and of the same type (1bd apt, studio, house, etc).
- a property that has an increasing property value and has a mortgage price that is less than
the average rent price for that neighborhood and type.

Unfortunately, this dataset does not contain the right information to find the average rent
price by neighborhood and property type.

For this project, I will focus on the properties with a below average sale price for their
neighborhood and type.
        

--- Data Cleaning ---
        
This dataset has many null or nonsensical values. To clean the data, I had to:
 - change the variable names for ease of use
 - removed nonsensical variables
 - change the sale_price, gross_sqft and land_sqft variables to be numeric and remove their null
 values
 - sale prices below 100,000 were removed as many properties had been passed down or inherited
 for $0 or sold to a family member for a low price. This also included the removal of 
 garage parking sales
 - change borough to the proper names
 - only include the top 100 neighborhood building class groups, with the largest amount of 
properties sold"
        
    )

    
    output$basictext <- renderText(
        "--- Basic Analysis ---
To do proper anaysis, I needed to account for the vast difference in price caused by the 
different neighborhoods and building classes. For example, a home in Chealsea and a home in 
Flushing would have a very different price, as would a one family home and three family home.
I added the following variables to account for this:
- avg_sale_price
- sd_sale_price
- avg_residential_units
- avg_commercial_units
- avg_total_units
- avg_land_sqft
- avg_gross_sqft
- avg_year_built

I took each observation and divided the corresponding variable value by its avg of that variable,
again, for its neighborhood and building class, to get another set of variables.
- ratio_sale_price
- ratio_residential_units
- ratio_commercial_units
- ratio_total_units
- ratio_land_sqft
- ratio_gross_sqft
- ratio_year_built

These ratio variables are what I later used in my exploratory data analysis.

Now with the variables, avg_sale_price and sd_sale_price, I assigned a value to each observation
based on whether it had a good price or not. If the property was more than one standard deviation
below its average sale price, sale_price < (avg_sale_price - sd_sale_price), it would have a 1 under
ther good_price variable. Otherwise, that value would be 0."
    )
    
    output$boxplot <- renderPlot(

        nyc_prop_for_EDA %>%
            mutate(., good_price = as.factor(good_price)) %>%
            select(., building_class_at_tos,
               contains(substr(input$numvariable, 7, 23)),
               good_price,
               input$numvariable) %>%
            ggplot() +
            geom_boxplot(aes(x = good_price,
                             y = nyc_prop_for_EDA[, input$numvariable],
                             color = good_price)) + 
            ylab(input$numvariable)

    )

    output$density <- renderPlot(

        nyc_prop_for_EDA %>%
            mutate(., good_price = as.factor(good_price)) %>%
            select(., building_class_at_tos,
                   contains(substr(input$numvariable, 7, 23)),
                   good_price) %>%
            ggplot() +
            geom_density(aes(x = nyc_prop_for_EDA[, input$numvariable], color = good_price)) + 
            xlab(input$numvariable)

    )
    
    output$neighborhood_p <- renderPlot(
        
        nyc_prop_for_EDA %>%
            mutate(., 
                   good_price = as.factor(good_price), 
                   neighborhood = ordered(neighborhood, levels =
                        c("BOROUGH PARK", "FLUSHING-NORTH", "BAYSIDE", "JACKSON HEIGHTS",
                          "BUSHWICK", "ST. ALBANS","FAR ROCKAWAY", "ARVERNE", "WILLIAMSBRIDGE", "PORT RICHMOND"), 
                      )) %>%
            group_by(., good_price, neighborhood) %>%
            summarise(., amount = n()) %>%
            group_by(neighborhood, .drop = TRUE) %>%
            summarise(., good_price = good_price, amount = amount, percent = amount/sum(amount)) %>%
            filter(., amount > 2) %>% head(20) %>%
            ggplot() + geom_col(aes(x = neighborhood, y = percent, fill = good_price), position = "dodge")  +
            coord_flip(ylim = c(0,1), expand = FALSE)
        
    )
    
    output$building_class_at_present_p <- renderPlot(
        
        nyc_prop_for_EDA %>%
            mutate(., 
                good_price = as.factor(good_price), 
                building_class_at_present = ordered(building_class_at_present, levels =
                    c("A9", "B3", "B1", "C0", "A1", "B2", "A2", "A5", "B9", "A0", "A3"))) %>%
            group_by(., good_price, building_class_at_present) %>%
            summarise(., amount = n()) %>%
            group_by(building_class_at_present, .drop = TRUE) %>%
            summarise(., good_price = good_price, amount = amount, percent = amount/sum(amount)) %>%
            ggplot() + geom_col(aes(x = building_class_at_present, y = percent, fill = good_price), position = "dodge")

        
    )
    
    output$zip_code_p <- renderPlot(
        
        nyc_prop_for_EDA %>%
            mutate(., 
                   good_price = as.factor(good_price), 
                   zip_code = ordered(zip_code,
                        levels = c("10303", "11377", "10456", "10306", "10466",
                                   "11221", "11358", "11210", "11361", "11355"))) %>%
            group_by(., good_price, zip_code) %>%
            summarise(., amount = n()) %>%
            group_by(zip_code, .drop = TRUE) %>%
            summarise(., good_price = good_price, amount = amount, percent = amount/sum(amount)) %>%
            filter(., amount > 2) %>% head(20) %>%
            ggplot() + geom_col(aes(x = zip_code, y = percent, fill = good_price), position = "dodge")
        
    )
    
    output$neighborhood_t <- renderTable(
        
        nyc_prop_for_EDA %>% 
                group_by(., good_price, neighborhood) %>%
                summarise(., amount = n()) %>%
                pivot_wider(., names_from = good_price, values_from = amount) %>%
                select(., neighborhood, bad_price = "0", good_price = "1") %>%
                mutate(., ratio_gp = good_price / bad_price) %>%
                arrange(., desc(ratio_gp))
        
    )
    
    output$building_class_at_present_t <- renderTable(
        
        nyc_prop_for_EDA %>%
            group_by(., good_price, building_class_at_present) %>%
            summarise(., amount = n()) %>%
            pivot_wider(., names_from = good_price, values_from = amount) %>%
            select(., building_class_at_present, bad_price = "0", good_price = "1") %>%
            mutate(., ratio_gp = good_price / bad_price) %>%
            arrange(., desc(ratio_gp))
        
    )
    
    output$zip_code_t <- renderTable(
        
        nyc_prop_for_EDA %>%
            group_by(., good_price, zip_code) %>%
            summarise(., amount = n()) %>%
            pivot_wider(., names_from = good_price, values_from = amount) %>%
            select(., zip_code, bad_price = "0", good_price = "1") %>%
            mutate(., ratio_gp = good_price / bad_price) %>% drop_na(.) %>%
            arrange(., desc(ratio_gp))
        
    )
    
    output$conclu <- renderText(
        
        "--- Conclusion ---
The variables in this dataset does have an influence on the price of a property, but they have
no influence on how profitable a property is. I took the variables and took into account the
neighborhood and building class (at time of sale) for each variable by dividing the variable by
its corresponding average, again, based on its neighborhood and building class. For example,
for a property's sale price, I would find all the properties that was in its neighborhood and
building class. Then take the average of their sale price and divide that property's sale price
by the average sale price I found.

I used this method with every grouping of neighborhood and building class and applied it to all
the numeric variables of all the properties. I also found the avg standard deviation of the
sale price. If a property had a sale price that was below one standard deviation less than its
average sale price, it would be considered a good priced property. I separated my dataset this
way into 2 groups, good price and bad price.

I did my exploratory data analysis. With all the numeric variables, I made boxplots and density
plots. Every single variable showed the same trend between the good price and bad price group.
This showed that these variables had no effect on having a 'good price'.


I also made bar graphs for each of the categorical variables. The variables had a similar ratio
of amount of good price to bad price properties. When comparing the top and bottom 5
observations, there is a more noticable difference. A few observations in each variable stood
higher than the others, which are as follows:

 - borough:         Bronx
 - neighborhood:    Port Richmond, Williamsburg
 - building class:  A9, B3
 - zip code:        10303, 11377
 
 The tables that were created did a better job in showing where not to by property. The lowest
 ratio_gp had terrible chances of having a profitable property.
 
These observations are where there is the most difference between the good price and bad price
groups. More analysis is required to know exactly how viable this information is."
    )
    output$futwork <- renderText(
        "If I had more time, I would gather the data from the last few year and see if I get similar
results. I believe that it would be different. There were not enough observations within the
good price group to get a strong result. I would also scrape housing websites to gather more
variables to compare to the good and bad price groups.

I would also like to try doing this a different way. I would calculate the sale price per gross
square foot and analyze all the variables based on this."
    )
    
}


