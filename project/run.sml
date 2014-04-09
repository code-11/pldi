structure Run =struct 

	structure P = Parser
	structure T= Translator

	fun run () = 
		let val tokenList= P.lexString("class HelloWorldApp {public static void main(String args) {System.out.println(); int blagh=5; double [] herp=[1.0,2.0]; float wargh=5.0; }}") in
			(P.printTokens tokenList);
			print (T.stringify (T.translate tokenList))
		end
end