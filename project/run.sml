structure Run =struct 

	structure P = Parser

	fun run () = P.printTokens (P.lexString("class HelloWorldApp {public static void main(String args) {Systemoutprintln(\"Hello\");    }}"))
end