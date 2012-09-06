


merge [] tail = tail
merge tail [] = tail
merge list1@(head1:tail1) list2@(head2:tail2)
    | head1 > head2 = head2:(merge list1 tail2)
    | otherwise     = head1:(merge tail1 list2)

sort_and_tail list n
    | n == 0 = ([],list)
    | n == 1 = ([head list],tail list)
    | otherwise =
        let half = n `div` 2 in
        let (sorted1,tail1) = sort_and_tail list half in
        let (sorted2,tail2) = sort_and_tail tail1 (n - half) in
              (merge sorted1 sorted2,tail2)

merge_sort l = fst $ sort_and_tail l $ length l

quick_sort_sub list pivot first_half second_half trailer =
    case list of
      [] ->
        let latter_half =
              case second_half of
                []        -> pivot:trailer
                otherwise -> (pivot:) $ quick_sort_sub (tail second_half) (head second_half) [] [] trailer
        in
        case first_half of
          []        -> latter_half
          otherwise -> quick_sort_sub (tail first_half) (head first_half) [] [] latter_half
      hd:tl ->
        if hd < pivot
        then quick_sort_sub tl pivot (hd:first_half) second_half trailer
        else quick_sort_sub tl pivot first_half (hd:second_half) trailer

quick_sort [] = []
quick_sort (hd:tl) = quick_sort_sub tl hd [] [] []




