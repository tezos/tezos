open Utils;;

assert(split ' ' "ab cd ef gh" = ["ab"; "cd"; "ef"; "gh"]);;
assert(split ' ' " ab  cd ef gh " = ["ab"; "cd"; "ef"; "gh"]);;
assert(split ' ' ~dup:false " ab  cd ef gh " =
       ["ab"; ""; "cd"; "ef"; "gh"]);;
assert(split ' ' ~dup:false ~limit: 3 " ab  cd ef gh " =
       ["ab"; ""; "cd"; "ef gh "]);;
assert(split ' ' ~dup:false "a  " = ["a"; ""]);;

let old_split delim ?(dup = true) ?(limit = max_int) path =
  let l = String.length path in
  let rec do_slashes acc limit i =
    if i >= l then
      List.rev acc
    else if String.get path i = delim then
      if dup then
        do_slashes acc limit (i + 1)
      else
        do_split acc limit (i + 1)
    else
      do_split acc limit i
  and do_split acc limit i =
    if limit <= 0 then
      if i = l then
        List.rev acc
      else
        List.rev (String.sub path i (l - i) :: acc)
    else
      do_component acc (pred limit) i i
  and do_component acc limit i j =
    if j >= l then
      if i = j then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if String.get path j = delim then
      do_slashes (String.sub path i (j - i) :: acc) limit j
    else
      do_component acc limit i (j + 1) in
  if limit > 0 then
    do_slashes [] limit 0
  else
    [ path ];;

let test_split delim ?(dup = true) ?(limit = max_int) s =
  assert(split delim ~dup ~limit s = old_split delim ~dup ~limit s);;

test_split '1' ~dup:true ~limit:87 "1fZ111dk111Rum19y4T11qdGa118d2Q111AsY111Ae2";;
test_split 'M' ~dup:false ~limit:85 "ycEUMMGjsiMMM8mlMYkTmMMFvMMM";;
test_split '4' ~dup:false ~limit:2 "44rR4443k7j4 5xen0HNx4GJrC44hqBn44YW98N444";;
test_split 'a' ~dup:true ~limit:50 "apa";;
test_split 'C' ~dup:false ~limit:92 "CCCXCC";;
test_split 'K' ~dup:false ~limit:72 "KKKqKKKQmKeKgUXKKKjAKKCSWSX7KK";;
test_split 'Y' ~dup:false ~limit:49 "YYLYoYkaZsYCcQ";;
test_split 'j' ~dup:false ~limit:28 "jjjTcjjjzL o YrjtgGHpjjj9GjjjFRj2Igpj";;
test_split 'B' ~dup:false ~limit:95 "BB";;
test_split '7' ~dup:false ~limit:37 "ros777XhM77J7sUmCRYiS7CEYU7479Vdr7t3777";;
test_split '9' ~dup:true ~limit:19 "uY99ek99W 9aWD199bpHY6";;
test_split 'X' ~dup:false ~limit:3 "9JarXXX5TXqMXXVXXXzCJPXXXltzE";;
test_split 'O' ~dup:false ~limit:33 "";;
test_split 'K' ~dup:false ~limit:76 "KKg4gkKK";;
test_split ' ' ~dup:false ~limit:39 "   h  dxYk   oALf 1   ZyHRzZ spJ9 ";;
test_split 'C' ~dup:false ~limit:92 "nCCjCxfCCCdoVnxICCC";;
test_split '2' ~dup:false ~limit:97 "222wLw2cwEC2224722NFy222raSH267ZtBbq";;
test_split 't' ~dup:true ~limit:2 "";;
test_split 'V' ~dup:false ~limit:7 "VVV";;
test_split '4' ~dup:true ~limit:11 "7HA4l444";;
test_split 'V' ~dup:false ~limit:80 "XeS7VVVL GVV4VVVTPvSVIEVuIbVVcVDaauVSV";;
test_split 'R' ~dup:false ~limit:4 "RRcZwb7IReMARRATwRRJseRROwS ";;
test_split 'Q' ~dup:false ~limit:40 "QQGPQ5lCyQhPQQQaQknhQ";;
test_split 'x' ~dup:true ~limit:10 "xmJxxkIxxdZxToDxDmBxAgE2xxpACxx3rtOxxx";;
test_split 'd' ~dup:false ~limit:59 "4adhI5dYzYN";;
test_split 'D' ~dup:false ~limit:58 "MLmmDDS3WDg88DDD";;
test_split 'e' ~dup:true ~limit:86 "CKIxreee6ce3eeecUWLMqee";;
test_split 'Q' ~dup:true ~limit:92 "QQ6tCQQQdHFGQQQ";;
test_split 'b' ~dup:false ~limit:83 "b";;
test_split 'C' ~dup:true ~limit:43 "C3CCC";;
test_split 'j' ~dup:false ~limit:74 "jjSjhQOGuPRjj";;
test_split 'g' ~dup:true ~limit:46 "HuWggpN0qOngzgglB9fxxkszwVgg4G0gg";;
test_split 'A' ~dup:true ~limit:5 "AAf8eARmszAAA";;
test_split 'C' ~dup:false ~limit:69 "CCCctCCqTCuKBCCDFE9CObXLCvYaRCC";;
test_split 'd' ~dup:true ~limit:39 "ddd";;
test_split 'A' ~dup:false ~limit:24 "AAAuKVAAA";;
test_split 'B' ~dup:true ~limit:68 "rLF6BBdKmBuC6DKPBaRfB4jBBBkRrdBBfCBB";;
test_split 'k' ~dup:false ~limit:17 "kMgVkkkMjU9kkktrVkkkUvAzkkWnUxkkAICHkkkd7zxkkik";;
test_split 'K' ~dup:false ~limit:68 "gKKKvRSKt7ijKKKxKKKvJp6KK7jKyPAKK";;
test_split 'n' ~dup:true ~limit:46 "nP4RnnnEbvbnns8K6nnn7P3nnnFBAnXxmYnnn";;
test_split '0' ~dup:true ~limit:57 "00Bd00";;
test_split 'V' ~dup:true ~limit:54 "";;
test_split 'C' ~dup:true ~limit:46 "CCCU5 wClaTJCwD0vo7CGjzsQC K4joDvCCCpC";;
test_split 'K' ~dup:false ~limit:87 "KKjiKKKzEKc0KK";;
test_split 'H' ~dup:true ~limit:84 "B3MHUHwHH";;
test_split 'E' ~dup:false ~limit:76 "E";;
test_split 'q' ~dup:false ~limit:47 "qqqByWHqmqqcqqeED2qqqQoF0q";;
test_split 'C' ~dup:true ~limit:25 "CCDYKL6CC3CCe4rDLPd3JkAZy gDxHCCqYBcCCC";;
test_split 'I' ~dup:false ~limit:10 "IIaIIbceOIZl0IIIYBmeUIII";;
test_split '3' ~dup:true ~limit:42 "33";;
test_split '2' ~dup:false ~limit:29 "222lKV2 Rf222mv22M8Kk2";;
test_split 'b' ~dup:false ~limit:33 "bVInYEbbxijObbW3utb8bbb24b";;
test_split 'r' ~dup:false ~limit:88 "rrrS3 rf8 zrrArr7gsrrnrrrqrryArrrdGckrx6rrE26Krrr";;
test_split 'g' ~dup:false ~limit:56 "ZBIgk56Fggfcg0gdLi0IEEg";;
test_split 'Q' ~dup:false ~limit:76 "z0IjAQQQ";;
test_split 'e' ~dup:false ~limit:47 "eee3VG0efeeeVEkXwvd3Wexe";;
test_split 'p' ~dup:false ~limit:98 "pppWYppp2dSbpLpppGXp";;
test_split '9' ~dup:true ~limit:11 "99feT99T9997999CFM9x9C9";;
test_split 'm' ~dup:true ~limit:94 "MA4mmmKXzbXmmmQksmmmokFmmmQrnmmjpHG5dcVZmUzO";;
test_split 'B' ~dup:false ~limit:68 "wcBePBm69BBTBB7KDBBBweraxyBBB";;
test_split 'g' ~dup:false ~limit:47 "tlaoggkNbPgggjMCIgNr2gggp gggrKggbbgg";;
test_split 'J' ~dup:false ~limit:28 "JJJZdi4JJNJJJ3aOIofYJJ9JaJJJvwB2gKJ";;
test_split '0' ~dup:true ~limit:100 "0VzQm00";;
test_split 'e' ~dup:false ~limit:19 "eeaeew3HaXVe";;
test_split 'z' ~dup:false ~limit:40 "zKeBzzzV zzxEzz2kezzVir zz";;
test_split 'm' ~dup:false ~limit:3 "m9VmmmN09RmmFzTsmmmlmtmmmVbZmmE9mmmOm";;
test_split '0' ~dup:true ~limit:24 "0oTh91qI00ZkPT000bH0OyXz5000u00";;
test_split 'R' ~dup:false ~limit:88 "RDs1XTPBORRR37";;
test_split 'k' ~dup:false ~limit:85 "S2yvkk";;
test_split 'D' ~dup:true ~limit:8 "DDiD";;
test_split 'A' ~dup:true ~limit:81 "9vcSAZuAAAQ1AAcAwJngqAgrJeAANZecA";;
test_split '5' ~dup:false ~limit:51 "55jr55u0Lfyg555";;
test_split '0' ~dup:true ~limit:81 "0co9cEP0003000K000";;
test_split 'A' ~dup:false ~limit:7 "60UAAAfXAxswWN6MAAgl7AAAE4CpAAAaV5AAAtWJAAAS9mA";;
test_split 'y' ~dup:true ~limit:93 "sly";;
test_split 'w' ~dup:true ~limit:56 "wwwGv1uwGvQwwwfhExXwwwTzFYwwDZ6Bww";;
test_split 's' ~dup:true ~limit:78 "sss D03HM80ssPYDwE";;
test_split '0' ~dup:false ~limit:83 "000U00JjPU0FTX0aE";;
test_split 'g' ~dup:false ~limit:52 "Xgg";;
test_split 'j' ~dup:true ~limit:7 "jjYzvjjjVx9Vjj";;
test_split 'G' ~dup:false ~limit:16 "GGhHCGMRe5GGGpv4rGGG";;
test_split 'C' ~dup:false ~limit:60 "CCLYS3Cs m9k9CCngZXC8CC37PVCCJwCCtI6C";;
test_split 'm' ~dup:false ~limit:79 "NYmmZulKiFmmaTqmmmX2HDmmmniT4mmmNqmm";;
test_split 'F' ~dup:true ~limit:14 "FFFVulBx0FFlF4FFFh2NTFFFPJPFuRq9M2eIzFs12yFF";;
test_split 'N' ~dup:false ~limit:72 "fOKNPpuqRiNNNj85";;
test_split '5' ~dup:true ~limit:54 "5hvLE5fV555XHs55C55jZd5";;
test_split 'H' ~dup:true ~limit:65 "HHNa4iHHFHHHJobZHHyoHHWHHH";;
test_split '9' ~dup:true ~limit:13 "99Alz9sp99FHel99";;
test_split '6' ~dup:false ~limit:36 "gn66f6";;
test_split 'z' ~dup:false ~limit:49 "zzzasXRozz";;
test_split 'J' ~dup:false ~limit:54 "JJO7";;
test_split '7' ~dup:false ~limit:14 "7FW777PqkcQ77xKm77tg877AIl777nymER7wj777";;
test_split '9' ~dup:false ~limit:39 "9";;
test_split 'B' ~dup:true ~limit:64 "lJ3OBwbh9BBp 6B";;
test_split 'O' ~dup:false ~limit:11 "OOOXcODxSOOmkOOAQfOIf7WOO";;
test_split 'B' ~dup:true ~limit:9 "BBVxIo5crWBBBl BB";;
test_split '7' ~dup:false ~limit:31 "77OnFlSgi77IcMXp76YG";;
test_split 'H' ~dup:true ~limit:86 "HHHnHH0HHLQHHHsHHBIUHHQYHjXbHHH9mSHH";;
test_split 'u' ~dup:false ~limit:77 "uuQKuuuUBdxuuBGtuuuzuuuiCZubAjEuuusq7uEButAmuuu";;
test_split 'a' ~dup:true ~limit:22 "aaCaXowaaa9jaaqJqat9 0aaeWZaaaKCca";;
