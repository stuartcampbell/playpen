To run test for most circumstances use

>> libisis_test('lite','continuous')

the test will run with smaller datasets and require no user input. 

DO NOT run the tests without the 'lite' option on desktop machines. 

for libisis data manipulations or graphics only use the options

>> libisis_test('lite','continuous','gtk only')
>> libisis_test('lite','continuous','libisis only')

type help libisis_test (and look inside the file) for more information. Add tests to either libisis_test_script or gtk_test_script. 