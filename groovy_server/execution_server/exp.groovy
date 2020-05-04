// println((true ? false : false ? false ? -0.193835296555 : 0.0165951853913 : (true ? -0.024181368253 : 0.110970617523)))

// new GroovyShell().evaluate("println((true ? false : false ? false ? -0.193835296555 : 0.0165951853913 : (true ? -0.024181368253 : 0.110970617523)))")

// groovy -e "println((true ? false : false ? false ? -0.193835296555 : 0.0165951853913 : (true ? -0.024181368253 : 0.110970617523)))"

// println(true ? true : false ? -2239671453694352915 * -2 : (-3069490355002432308 * -6))
// println((true ? true : false) ? -2239671453694352915 * -2 : (-3069490355002432308 * -6))
// println(true ? true : (false ? -2239671453694352915 * -2 : (-3069490355002432308 * -6)))

// println(false ? 2 : 2 + 3)
// println(true ? 2 : 2 + 3)

// int IxtZdSQvU = 5;
// int bOSSVvSfWin = 5;
// println(-2.71001523862e-05 <= -1.25602012579e-06 ? -0.0123637145528 - 2696430.30465 : IxtZdSQvU ** bOSSVvSfWin) >= -0.00223097106079 ** 5.9577220852e-05 ** (-1.14974239542 * 0.0543030818856);

// try {
//     new GroovyShell().evaluate("5/0")
// } catch (ArithmeticException) {
//     println("Well")
// }

// def socketServer = new ServerSocket(5000)

// socketServer.accept { socket ->
//     socket.withStreams { input, output ->

//         def reader = input.newReader()
//         def shell = new GroovyShell()
//         while(true) {
//             def program = reader.readLine()
//             try {
//                 def result = shell.evaluate(program)
//                 output << 0         // Success
//                 output << result    // Value
//             } catch (ArithmeticException) {
//                 output << 1         // Ignore arithmetic exceptions
//                                     // No value
//             } catch (Throwable) {
//                 output << 2         // Failure
//                                     // No value
//             }
//         }
//     }
// }

//println(-160823 ** (7 ** 9))
// Thread t = new Thread(new Runnable() {
//     public void run() {
//         try {
//             println(-160823 ** (7 ** 9));
//         } catch(InterruptedException e) {
//             e.printStackTrace();
//         }
//     }
// })

// t.start();
// t.join(5000);
// t.stop();
// println("Done");
// println((("nS>9kV2P Ebk:UYe" / 7 ? 0.470475296746 * 4.80503915625 : -1.0731050047 * 0.000160071736445) != -1747188.53699 + 5.75361843407e-07 - -1257730.11765 * -5.66590115782 ? (true ? -2494.63153861 : -0.0699776389919) < (false ? -2.54238530344e-06 : 0.192518258169) ? 82 + 4611686018427387903 <= (false ? 3 : 1) : (true ? true : true) ? true ? false : true : false ? true : false :  !(-3 >= -4611686018427387904) ? (true ? true : false) ? 5 != 7 : 0 == 1704936993961727879 : false) ? -2906809583925759724 * -7 * (-8 - 411581216446470768) / (-9 == 6 ? -5 / 1 : 0 / -5) + ((false ? false : true) ? 4611686018427387903 + -31 : false ? 7 : 62) / (-3 + (false ? 9 : 0)) : ((false ? true : true) ? -3817734821330578021 * -2 : -4611686018427387904) * (9 * -6 * (false ? -6 : 2709435595571810408)) + ((-3.2686743285e-06 != -16.5438793262 ? 7 * 0 : false ? -4 : 0) + (1910090.40887 != -3.05645399916e-06 ? -9 - 2 : 2 - -1)))
// try {
//     new GroovyShell().evaluate("\"H\" / 5")
// } catch (ArithmeticException a) {
//     println("Div 0")
// } catch (MissingMethodException b) {
//     println("Failure")
// }

// println(2 ** 2147483648)


import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.control.customizers.ASTTransformationCustomizer
import groovy.transform.TypeChecked
import groovy.transform.CompileStatic

def config = new CompilerConfiguration()
config.addCompilationCustomizers(
    new ASTTransformationCustomizer(
        CompileStatic,
        extensions:["NumberTypeCheckExtensions.groovy"])
)
// def shell = new GroovyShell(config)
def shell = new GroovyShell(config)
// shell.evaluate("-0 ** 5")
shell.evaluate("(true ? 0 : 146.17245898) >= 0 ** 0 ? 0 : 0")
// shell.evaluate("\"h\".myOwnFunction()")
// shell.evaluate("hello()")
