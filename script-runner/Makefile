dist/build/poc/poc: AutomatedTestRunner.hs BlockParser.hs Poc.hs BrickUI.hs
	runhaskell Setup.hs build -j10 poc

poc-test: dist/build/poc/poc
	mkdir -p ~/dedup/poc-state
	./dist/build/poc/poc --configuration-file ../lib/configuration.yaml --configuration-key mainnet_dryrun_full --log-console-off --db-path ~/dedup/poc-state/db --keyfile ~/dedup/poc-state/secret.key --log-config ../log-configs/connect-to-cluster.yaml --logs-prefix ~/dedup/poc-state/logs/ --topology topology-staging.yaml

.PHONY: poc-test
