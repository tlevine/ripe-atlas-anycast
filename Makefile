anycast.csv: anycast.json
	./to-csv.py anycast.json > anycast.csv
anycast.json:
	wget -O anycast.json http://sprunge.us/TLOV
