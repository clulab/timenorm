from timenorm_data_provider import TimeDataProvider
from distance_utils import *

data_provider = TimeDataProvider(corpus_dir="/Users/bulut/timenorm-garage/tempeval-2013-replicate")
test_input_path = "/Users/bulut/timenorm-garage/tempeval-2013-replicate/Test"

test_output_path = "/Users/bulut/timenorm-garage/tempeval-2013-replicate/Test-Modified"



modify_gold_test_data(test_input_path, test_output_path)






