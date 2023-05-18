clean:
	rm -f ./data/raw/*
	rm -f ./data/analysis/*

get_data:
	Rscript ./src/get_data.R

clean_data:
	Rscript ./src/clean_data.R

run_report:
	Rscript ./src/run_report.R

run_pipeline:
	make clean
	make get_data
	make clean_data
	make run_report
