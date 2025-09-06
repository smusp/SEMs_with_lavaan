
### Data for Tables 21.1, 21.2, 21.3, 21.4 ###

## Reshape data - long to wide
tab <- 0.5 * table(df$x)  # in each condition
df$id <- c(rep(1:tab[1], 2), rep(1:tab[2], 2), rep(1:tab[3], 2))  # id variable 

df <- reshape(df, timevar = "c", idvar = c("id", "x", "g"), varying = c("pre", "y"), 
   direction = "wide")

df <- within(df, {
## Grand mean centered "pre" - the before scores
   preC <- scale(pre, scale = FALSE)

## Drop the id variable
   id <- NULL

## Gender X Coping Strategy interaction
  sg <- interaction(x, g, sep = "")

## Dummy variables to use in regression analysis
## Dummy variables for "Coping Startegy"
   dummies1 <- model.matrix(~ x - 1)

## Dummy variables for interaction
   dummies2 <- model.matrix(~ sg - 1)
})

## Unnest the nested 'dummies' matrices, and rename their colomns
df <- do.call(data.frame, df)
names(df) <- gsub("dummies1.x", "", names(df))
names(df) <- gsub("dummies2.sg", "", names(df))
