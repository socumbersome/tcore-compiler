
type nameSupply = int;;
let initialSupply = 0;;

let makeName prefix n = prefix ^ "_" ^ string_of_int n;;

let getName name_supply prefix = 
	(name_supply + 1, makeName prefix name_supply);;

let getNames name_supply prefixes =
	let n = List.length prefixes
	in (name_supply + n, List.map2 makeName prefixes
		(Lists.range name_supply (name_supply + n - 1)));;
