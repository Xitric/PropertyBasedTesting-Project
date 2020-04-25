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

            def shellResult = new ShellResult()
            def shellThread = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        shellResult.result = shell.evaluate(program)
                    } catch (ArithmeticException) {
                        // Ignore
                    } catch (Throwable) {
                        shellResult.errored = true
                    }
                }
            })
            shellThread.start()
            shellThread.join(1000)  // Thread hangs if one second passes with no result
            shellThread.stop()      // Please kill me for committing this unholy sin

            if (shellResult.errored) {
                output << 2         // Failure
                                    // No value
            } else if (shellResult.result == null) {
                output << 1         // No failure, but also no value
            } else {
                output << 0         // Success
                // output << result    // Value
                // output << (char) 3  // End of transmission
            }
        }

        println("Goodbye")
    }
}

class ShellResult {
    Object result;
    boolean errored;
}
