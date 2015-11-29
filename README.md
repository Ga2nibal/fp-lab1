<b>ФП, лаб 1</b>
	
	Выполнил Сергей Федюкович (151004)
	
	<b>Применение:</b>
	```
	Available options:  Help Options:
						-h, --help	Show option summary.
						--help-all 	Show all help options.

						Application Options:
						--inputFile :: text ->  Input filepath	-> default: ""
						--outputFile :: text -> Output filepath	-> 	default: ""	-> 
						--numOfClaster :: int -> number of clusters	-> default: 0
						--accuracy :: float64  -> accuracy -> default: 1.0e-4
						--metric :: metric[Euclid,Hemming] -> Method of distance cuclulating ->  default: Euclid
						--isRandCenters :: bool  -> Method of initialize: false=="RandomMatrix" or true=="RandomCenters" -> default: false
						--separator :: text  -> column separator(will select the first character of the string)  -> default: ","
						--isIgnoreHeader :: bool  -> Ignore header row  -> default: false
						--isIgnoreFirst :: bool  -> Ignore first column  -> default: false
						--isIgnoreLast :: bool  -> Ignore last column  -> default: false
	  ```
	  
	  # Запуск с помощью sandbox
	```bash
	git clone https://github.com/Ga2nibal/fp-lab1.git
	cd fp-lab1
	cabal sandbox init
	cabal install --only-dependencies
	cabal build
	./dist/build/lab1/lab1 --inputFile="..\..\..\dataInput\irises.txt" --isIgnoreLast=true --isRandCenters=true --numOfClaster=3
	```