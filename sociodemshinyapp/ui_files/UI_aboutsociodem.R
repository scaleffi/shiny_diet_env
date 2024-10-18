tabItem(
  #tabName = "sociodem",
  # tabItem(
  tabName = "about_sociodem", 
  box(width = 12,
      title = "About this data", HTML(
        "These tabs allow you to compare food-related environmental footprints across different sociodemographics.
              Use the first tab, 'Sex and age', if you want to explore how food-related environmental footprints differ among sexes and age groups, relative to the global
              or regional average. Open the second tab instead, 'Edu-urb', if you want to explore how the footprints change across education levels and urban/rural residence, 
              relative to the global or regional average.<br><br> 
              In both tabs, the values can be interpreted as percentages which show how much higher or lower the impact of a 
              specific group is, compared to a global or regional average - set at 100%. For example, a value of 137 corresponds to a diet that is 37% higher than the mean.
              A value of 81 corresponds to a diet that is 19% lower than the mean.<br><br>
              If you are interested in seeing how absolute and relative impacts differ across sociodemographics, regions, and environmental dimensions, open the third and fourth tab.<br><br>
              In all tabs, you can visualise the data through two measures: actual demand, or demand normalised to 2,000 kcal/day. The first measure, actual demand, shows the footprints
              based on the real per person demand estimated for the underlying food. But comparing environmental footprints across sociodemographics using only this measure
              may be misleading: children, for example, eat less than adults in absolute terms, making their footprints invariably smaller in absolute terms. To control for these biophysical factors, we also calculated footprints
              based on demand normalised to 2,000 kcal/day for all groups, while maintaining dietary composition. This means that by selecting this second
              measure in the input parameters, users can, for example, compare the food-related environmental footprints of children between 0-10 and adults
              aged 20-39 as if they were both eating 2,000 kcal/day - but maintaining their respective proportional dietary compositions. Many large differences across sexes and age groups
              disappear or lose significance when using this measure of normalised demand, suggesting that they may simply be driven by the different
              energy requirements of individuals in each group. 
              "
        )
  )
)