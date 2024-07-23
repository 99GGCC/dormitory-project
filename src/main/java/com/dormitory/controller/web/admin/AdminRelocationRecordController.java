package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.RelocationRecordQry;
import com.dormitory.controller.vo.RelocationRecordVO;
import com.dormitory.service.RelocationRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 动迁记录表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/relocation/record")
@Api(value = "AdminRelocationRecordController", tags = {"动迁记录接口"})
public class AdminRelocationRecordController {

    /**
     * 动迁记录Service
     */
    private final RelocationRecordService relocationRecordService;

    @ApiOperation("动迁记录分页查询")
    @GetMapping("/page")
    public R<IPage<RelocationRecordVO>> pageByQry(RelocationRecordQry qry) {
        return R.success(relocationRecordService.pageByQry(qry));
    }
}

