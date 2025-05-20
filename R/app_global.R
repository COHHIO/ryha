# Read _brand.yml
brand <- yaml::read_yaml(app_sys("_brand.yml"))

# Store color palette in a global object
palette <- brand$color$palette
