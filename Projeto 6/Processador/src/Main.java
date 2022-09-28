import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;


/**
 * Principios de Programação
 * Trabalho 6
 * André da Silva Proença 53370
 * Nuno Fontes 46413
 * 
 * Compile:  cd Processador && javac src/*.java -d bin && cd ..
 * Run: java -cp Processador/bin Main metricas.txt dataset.csv Agregador/Main
 */
public class Main {

	public static void main(String[] args) {

		//Parametros de entrada
		//Main metrics.txt dataset.csv Agregador/Main
		String metricasFile = args[0];
		String dataSetFile = args[1];
		String agregadorPath = args[2];

		Supplier<Stream<String>> metricasSuplier;
		Supplier<Stream<String>> transactionsSuplier;


		//Tratar os parametros de entrada
		metricasSuplier = () -> {
			try {
				return readAndProcessMetricas(metricasFile);
			} catch (IOException e) {
				System.out.println("Erro!\nFicheiro txt Inexistente\nConteúdo do "
						+ "ficheiro incompatível.");
			}
			return null;
		};
		transactionsSuplier = () -> {
			try {
				return readAndProcessDataSet(dataSetFile);
			} catch (IOException e) {
				System.out.println("Erro!\nFicheiro csv Inexistente\nConteúdo do "
						+ "ficheiro incompatível.");
			}
			return null;
		};
		

		List<String> outputListOfEachAgregatorSequentially = 
				unpackAndProcessAgregadores(metricasSuplier, transactionsSuplier, agregadorPath);

		List<List<String>> groupedTransactionsOutput = 
				groupTransactionsOutput(outputListOfEachAgregatorSequentially, metricasSuplier);
		
		List<List<String>> transactionList = trasposeTransactionsOutput(groupedTransactionsOutput);
		
		System.out.println(formatTransactionsOutput(transactionList));
		
		//transactionList to File our_output.txt
		try {
			File file = new File("our_output.txt");
			if(!file.exists()){
				file.createNewFile();
			}
			PrintWriter pw = new PrintWriter("our_output.txt");
			pw.print(formatTransactionsOutput(transactionList)+"\n");
			pw.close();
		} catch (FileNotFoundException e) {
			System.out.println("Erro!\nFicheiro Inexistente");
		} catch (IOException e) {
			System.out.println("Erro!\nFicheiro Inexistente");
		}
	}

	/**
	 * Lê todos as metricas de um dado ficheiro txt para um stream.
	 * @param metricasFile ficheiro txt com as metricas a processar.
	 * @return um stream com as metricas.
	 * @throws IOException
	 * 
	 * Exemplo: readAndProcessMetricas(metricas.txt)
	 * Output: [sum 1, average 1, maximum 1, average 1 groupby 2, average 1 groupby 2 groupby 3,
	 * 			average 1 groupby 3 groupby 2] 
	 */	    
	public static Stream <String> readAndProcessMetricas(String metricasFile) throws IOException {
		//Ler Metricas.txt
		Stream <String> metricas = Files.lines(Paths.get(metricasFile));
		return metricas;
	}

	/**
	 * Lê todos as transactions de um dado ficheiro csv para um stream e 
	 * substitui as vírgulas de cada transaction por um espaço.
	 * @param dataSetFile ficheiro csv com as transactions a processar.
	 * @return um stream dos dados separados por espaços.
	 * @throws IOException
	 * 
	 * Exemplo: readAndProcessDataSet(dataset.csv)
	 * Output: [0 435 17 39, 1 3879 0 1, 2 3943 14 6, 3 4662 2 19, ...]
	 */
	public static Stream <String> readAndProcessDataSet(String dataSetFile) throws IOException {
		//Ler DataSet.csv
		Stream <String> transactions = Files.lines(Paths.get(dataSetFile));
		return transactions.map(transaction -> transaction.replace(",", " "));
	}

	/**
	 * Método que cria vários agregadores, um para cada métrica, fornece 
	 * input de metricas e transactions a partir dos supliers metricasSuplier e transactionsSuplier
	 * e empacota todos os outputs numa lista de agregadores.
	 * @param metricasSuplier um suplier que produz streams de metricas.
	 * @param transactionsSuplier um suplier que produz streams de transactions.
	 * @param agregadorPath o caminho para o binário do Agregador.
	 * @return listAgregador uma lista de output de transactions em ordem sequecial de acordo com
	 * cada metrica. Ou seja para cada métrica temos tantos outputs quanto o numero de transactions.
	 * Isto de forma sequecial, para a metrica sum 1 temos as 200 transactions sequenciais, depois para 
	 * a metrica average 1 temos outras 200 transactions sequenciais e assim sucessivamente.
	 * 
	 * Raciocionio de Output: [todas as transactions da metrica1 todas as transactions da metrica2 
	 * 						   todas as transactions da metricaN]
	 * Output: [435.0, 4314.0, 8257.0, ...N]
	 */
	public static List<String> unpackAndProcessAgregadores(Supplier<Stream<String>> metricasSuplier, 
			Supplier<Stream<String>> transactionsSuplier, String agregadorPath) {

		List<String> outputListOfEachAgregatorSequentially = new ArrayList<>();

		metricasSuplier.get().forEach(metrica -> {
			//Usar ProcessWrapper para o agregador
			ProcessWrapper pw = new ProcessWrapper(agregadorPath);
			pw.writeLine(metrica);
			transactionsSuplier.get().forEach(transaction -> {
				pw.writeLine(transaction);
				outputListOfEachAgregatorSequentially.add(pw.readLine());
			});
			pw.kill();
		});

		return outputListOfEachAgregatorSequentially;	
	}

	/**
	 * Método que agrupa os output de cada métrica numa sublista de uma lista.
	 * @param transactionsList a lista de transactions
	 * @param metricasSuplier um suplier que produz streams de metricas.
	 * @return uma lista de listas de transactions agrupadas de acordo com a sua métrica
	 * Por exemplo a primeira sublista corresponde às 200 transactions da metrica sum 1 e 
	 * assim sucessivamente para as outras sublistas.
	 * 
	 * Raciocionio de Output: [[todas as transactions da metrica1 dentro desta sublista], 
	 * 						   [todas as transactions da metricaN dentro desta sublista]]
	 * 
	 * Output: [[435.0, 4314.0, 8257.0, ...N],[435.0, 2157.0, 2752.3333, ...N]]
	 */
	public static List<List<String>> groupTransactionsOutput(List<String> transactionsList, 
			Supplier<Stream<String>> metricasSuplier) {
		
		//Agrupar o output das métricas numa lista de listas 
		//(Uma sublista da lista representa o output de uma métrica)
		
		//Formula: outputs / metricas = elemSubSize 
		//tal que outputs = nº outputs, metricas = nº metricas, elemSubSize = nº elem numa sublista
		int outputs = transactionsList.size();
		int metricas = (int) metricasSuplier.get().count();
		int elemSubSize = outputs / metricas;
		
		List<List<String>> groupedTransactions = 
				IntStream.range(0, metricas)
				.mapToObj(t -> transactionsList.subList(t * elemSubSize, (t + 1) * elemSubSize))
				.collect(Collectors.toList());
		
		return groupedTransactions;
	}
	
	/**
	 * Executa a "matriz" transposta de uma lista de listas 
	 * @param transactionList uma uma lista de listas de transactions.
	 * @return List<List<String>> uma lista de listas de transactions transposta.
	 * 
	 * Output: [[435.0, 435.0, 435.0, 435.0, 435.0, 435.0], 
	 * 		    [4314.0, 2157.0, 3879.0, 3879.0, 3879.0, 3879.0],[...N]]
	 */
	public static List<List<String>> trasposeTransactionsOutput(List<List<String>> transactionList) {
		
		//Lista de listas transposta
		List<List<String>> transactionListTransposed = IntStream.range(0, transactionList.get(0).size())
				.mapToObj(i -> transactionList.stream().map(j -> j.get(i)).collect(Collectors.toList()))
				.collect(Collectors.toList());
		
		return transactionListTransposed;
	}
	
	/**
	 * Formata uma lista de listas de transactions para o formato de output pretendido.
	 * @param transactionList lista de listas de strings que representam as transactions.
	 * @return String o formato de output pretendido.
	 * 
	 * Output: 435.0,435.0,435.0,435.0,435.0,435.0
	 * 		   4314.0,2157.0,3879.0,3879.0,3879.0,3879.0
	 * 		   ...N
	 */
	public static String formatTransactionsOutput(List<List<String>> transactionList) {
		
		//Formatar stream para o formato de output
		String outputTransactions = transactionList.stream().map(Object::toString)
				.map(s -> s.replace("[", ""))
				.map(s -> s.replace("]", ""))
				.map(s -> s.replace(" ", ""))
				.collect(Collectors.joining("\n"));
		
		return outputTransactions;
	}
	
}


















