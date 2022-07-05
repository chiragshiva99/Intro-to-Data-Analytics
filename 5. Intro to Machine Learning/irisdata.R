head(iris)
str(iris)
tree.iris= tree(Species~., data = iris)
summary(tree.iris)

iris_pre<-predict(tree.iris, type = "class")
able(iris_pre, iris$Species)

plot(tree.iris)
text(tree.iris, pretty=0)
