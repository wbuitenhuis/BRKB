df1 = data_frame(a = c(1,2,3), b= c(4,5,6))
df2 = data.frame(c = c(1,1,1,1), d = c(0,0,0,0))
df1 |> dplyr::left_join(df2, by = c("a"="c"))

temp3 <- dplyr::left_join(temp2, elements, by=c("parentId"="elementId")) |> 
  dplyr::arrange(id)

temp3 <- dplyr::left_join(temp2 |> dplyr::select(elementId, parentId), 
                          elements, by=c("parentId"="elementId")) |> 
        dplyr::arrange(id)
# join elements to temp2. match elementId from elements to parentId from temp2

temp3 <- temp2 |>
  dplyr::select(elementId, parentId) |>
  dplyr::left_join(elements, by=c("parentId"="elementId"))
                   

x <- temp2[2, ]|> dplyr::select(elementId, parentId)
match(temp)
match(temp2$parentId[2], elements$elementId)
(elements$elementId == temp2$parentId[2]) |> sum()
temp2$parentId[2]
temp2$parentId == elements$elementId[1]

match(temp2$elementId[2], elements$parentId)
match(elements$parentId[2], temp2$elementId)
match(elements$elementId[2], temp2$parentId)