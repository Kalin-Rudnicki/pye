
require 'erb'
require 'fileutils'

class String

  def capitalize
    self.empty? ?
      self :
      self[0].upcase + self[1..-1]
  end

end

class ProjectConfig

  attr_reader :project_name, :base_package

  def initialize(project_name, base_package)
    @project_name = project_name
    @base_package = base_package
    nil
  end

  def config_binding
    binding
  end

  def project_dir(*dirs)
    File.join(@project_name, *dirs)
  end

end

def get_config
  def get_arg(label)
    if (from_argv = ARGV.shift).nil?
      STDOUT.write("    #{label}: ")
      STDIN.readline.strip
    else
      STDOUT.puts("    #{label}: #{from_argv}")
      from_argv
    end
  end

  puts("Creating new template project...")
  puts("--- Formatting ---")
  puts("    Project Name: my-project-name")
  puts("    Base Package: my.base.package (if blank, defaults to project name 'myProjectName')")
  puts
  puts("--- Variables ---")
  project_name = get_arg("Project Name")
  base_package = get_arg("Base Package")
  puts

  ProjectConfig.new(
    project_name,
    (base_package.empty? || base_package == '--') ? project_name.gsub(/-(\w)/){$1.upcase} : base_package,
  )
end

def new_project(config)
  def create_project(config, verbose)
    def template_dir(dir)
      File.join(__dir__, 'files', dir)
    end
    def src_dir(config, sub_project)
      config.project_dir(config.project_name, sub_project, 'src', 'main', 'scala', config.base_package.gsub(/\./, "/"))
    end

    def replace_erb(str, config)
      ERB.new(str).result(config.config_binding)
    end
    def migrate(config, template_dir, project_dir, verbose)
      def log(label, from, to)
        puts("#{label} : #{from.split("/pye/template/files/", 2)[1]} -> #{to}")
      end

      FileUtils.mkdir_p(project_dir)
      (Dir.children(template_dir).filter { |f| !['.', '..'].include?(f) }).each do |f|
        template_child = File.join(template_dir, f)

        if File.directory?(template_child)
          migrate(config, template_child, File.join(project_dir, f), verbose)
        elsif File.extname(template_child) == '.erb'
          to = File.join(project_dir, f[0..-5])
          log('Templating', template_child, to) if verbose
          File.write(to, replace_erb(File.read(template_child), config))
        else
          to = File.join(project_dir, f)
          log('Copying   ', template_child, to) if verbose
          File.write(to, File.read(template_child))
        end
      end
    end

    migrate(config, template_dir('root'), config.project_dir, verbose)
    migrate(config, template_dir('js'), src_dir(config, 'js'), verbose)
    migrate(config, template_dir('jvm'), src_dir(config, 'jvm'), verbose)
    migrate(config, template_dir('shared'), src_dir(config, 'shared'), verbose)

    (Dir.children(config.project_dir('scripts')).filter { |f| !['.', '..'].include?(f) }).each do |f|
      system('chmod', '+x', config.project_dir('scripts', f))
    end
  end

  puts("--- Generating Project ---")
  force = ARGV.include?('-f')
  verbose = ARGV.include?('-v')
  if File.exist?(config.project_name)
    if force
      FileUtils.rm_rf(config.project_name)
      create_project(config, verbose)
    else
      puts("File already exists: #{config.project_name}")
    end
  else
    create_project(config, verbose)
  end
end

new_project(get_config)
