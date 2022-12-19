<?php

declare(strict_types=1);

namespace Kkeundotnet\Kkmarkdown;

class Kkmarkdown
{
    private bool $rss = false;
    private bool $unsafe = false;

    public function __construct(
        private readonly string $bin_path,
    ) {
    }

    public function set_rss(): self
    {
        $this->rss = true;
        return $this;
    }

    public function set_unsafe(): self
    {
        $this->unsafe = true;
        return $this;
    }

    private function get_cmd(): string
    {
        $cmd = $this->bin_path;
        if ($this->rss) {
            $cmd .= ' --rss';
        }
        if ($this->unsafe) {
            $cmd .= ' --unsafe';
        }
        return $cmd;
    }

    public function trans(string $md): string
    {
        $descriptorspec = [
            0 => ['pipe', 'r'], // stdin is a pipe that the child will read from
            1 => ['pipe', 'w'], // stdout is a pipe that the child will write to
            2 => ['file', '/tmp/kkmarkdown.log', 'a'] // stderr is a file to write to
        ];
        $process = proc_open($this->get_cmd(), $descriptorspec, $pipes);
        if (is_resource($process)) {
            // $pipes now looks like this:
            // 0 => writable handle connected to child stdin
            // 1 => readable handle connected to child stdout
            fwrite($pipes[0], $md);
            fclose($pipes[0]);

            $result = stream_get_contents($pipes[1]);
            fclose($pipes[1]);

            $return_value = proc_close($process);
            if ($return_value === 0) {
                return $result;
            }
        }
        die("kkmarkdown failed: trans");
    }

    /**
     * @deprecated Use trans instead
     */
    public function transform(string $md): string {
        return trans($md);
    }

    public function trans_from_file(string $file): string
    {
        $contents = file_get_contents($file);
        if ($contents !== false) {
            return $this->trans($contents);
        }
        die("kkmarkdown failed: trans_from_file");
    }

    /**
     * @deprecated Use trans_from_file instead
     */
    public function transform_from_file(string $file): string {
        return trans_from_file($file);
    }
}
