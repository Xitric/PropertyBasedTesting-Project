def socketServer = new ServerSocket(5000)

socketServer.accept { socket ->
    socket.withStreams { input, output ->

        def reader = input.newReader()
        def shell = new GroovyShell()
        while(true) {
            def program = reader.readLine()
            println("Got: ${program}")

            if (program.equals("kill")) {
                break;
            }

            try {
                def result = shell.evaluate(program)
                output << 0         // Success
                // output << result    // Value
                // output << (char) 3  // End of transmission
            } catch (ArithmeticException) {
                output << 1         // Ignore arithmetic exceptions
                                    // No value
            } catch (Throwable) {
                output << 2         // Failure
                                    // No value
            }
        }

        println("Goodbye")
    }
}
