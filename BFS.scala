import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.io.Source
import java.io._
import scala.collection.mutable.Queue

class Vertex
{
    var char: Char = '0'
    var color: Int = 0
    def this(char: Char, color: Int)
    {
        this()
        assert(color >= 0 && color <= 2)
        this.char = char
        this.color = color
    }
    def == (that: Vertex): Boolean = 
    {
        return (this.char == that.char) && (this.color == that.color)
    }
    override def toString(): String = 
    {
        return char.toString + ", color = " + color.toString
    }
}

class Edge
{
    var a: Vertex = new Vertex()
    var b: Vertex = new Vertex()
    def this(a: Vertex, b: Vertex)
    {
        this()
        this.a = a
        this.b = b
    }
    override def toString(): String = 
    {
        return this.a.toString + " --- " + this.b.toString
    }
}

class Graph
{
    var vertices: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]()
    var edges: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
    override def toString(): String =
    {
        var result: String = ""
        result = "Vertices:\n"
        for (i <- 0 to vertices.length-1)
        {
            result = result + vertices(i) + ", "
        }
        result = result + "\n"
        result = result + "Edges:\n"
        for (i <- 0 to edges.length - 1)
        {
            result = result + edges(i).toString + "\n"
        }
        return result
    }
}

class BFS
{
    var vertices: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]()
    var map: Map[Vertex, ArrayBuffer[Vertex]] = Map[Vertex, ArrayBuffer[Vertex]]()
    def this(fileName: String)
    {
        this()
        val inputFile = new File(fileName)
        if (!inputFile.exists())
        {
            println(fileName + " does not exist. ")
            System.exit(-1)
        }
        val source = Source.fromFile(inputFile)
        for (line <- source.getLines)
        {
            val array = line.split(" ")
            val vertex = new Vertex(array(0)(0), 0)
            vertices.append(vertex)
            var adj: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]()
            for (i <- 1 to array.length - 1)
            {
                adj.append(new Vertex(array(i)(0), 0))
            }
            map(vertex) = adj
        }
        source.close()
    }

    def search(source: Vertex): Map[Char, Int] = 
    {
        var distances: Map[Char, Int] = Map[Char, Int]()
        assert(vertices.contains(source))
        var Q: Queue[Vertex] = new Queue[Vertex]()
        Q.enqueue(source)
        distances(source.char) = 0
        def findIndex(array: ArrayBuffer[Vertex], ele: Vertex): Int = 
        {
            for (i <- 0 to array.length - 1)
            {
                if (ele.char == array(i).char) return i
            }
            return -1
        }
        while(Q.size > 0)
        {
            val u = Q.dequeue()
            val adj = map(u)
            for (ele <- adj)
            {
                var index = findIndex(vertices, ele)
                if (vertices(index).color == 0)
                {
                    Q.enqueue(vertices(index))
                    vertices(index).color = 1
                    distances(vertices(index).char) = distances(u.char) + 1
                }
            }
            u.color = 2
        }
        return distances
    }

    def printGraph(): Unit = 
    {
        for (key <- map.keys)
        {
            print(key + ": ")
            for (ele <- map(key))
            {
                print(ele + ", ")
            }
            print("\n")
        }
    }
}

object BFS
{
    def main(args: Array[String]): Unit = 
    {
        val bfs = new BFS("graph.txt")
        var vertices = bfs.vertices
        var source = new Vertex()
        for (v <- vertices)
        {
            if (v.char == 's')
            {
                source = v
            }
        }
        val distances = bfs.search(source)
        for (key <- distances.keys)
        {
            println(key + " -> " + distances(key))
        }
    }
}
